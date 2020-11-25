use std::alloc::{Layout, alloc_zeroed};
use std::mem::{transmute, size_of, align_of};
use std::ops::{Deref, DerefMut, Add, AddAssign, Sub};
use std::ptr::NonNull;
use std::slice;

// TODO: Python None = ptr::null?

struct Granule(usize);

// ---

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct GSize(usize);

impl Add for GSize {
    type Output = Self;

    fn add(self, other: Self) -> GSize { GSize(self.0 + other.0) }
}

impl AddAssign for GSize {
    fn add_assign(&mut self, other: Self) { *self = *self + other }
}

impl Sub for GSize {
    type Output = Self;

    fn sub(self, other: Self) -> GSize { GSize(self.0 - other.0) }
}

impl GSize {
    fn in_granules(size: usize) -> Option<GSize> {
        Some(GSize(size.checked_add(size_of::<Granule>() - 1)? & !(size_of::<Granule>() - 1)))
    }

    fn of<T>() -> Option<GSize> { GSize::in_granules(size_of::<T>()) }

    fn in_bytes(self) -> usize { self.0 * size_of::<Granule>() }
}

// ---

#[derive(Debug, Eq)]
struct Gc<T>(*const T);

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self { *self }
}

impl<T> Copy for Gc<T> {}

impl<T> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool { self.0 == other.0 }
}

impl<T> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &T { unsafe { transmute(self.0) } }
}

impl<T> DerefMut for Gc<T> {
    fn deref_mut(&mut self) -> &mut T { unsafe { transmute(self.0) } }
}

impl<T> Gc<T> {
    unsafe fn from_raw(raw: *mut T) -> Gc<T> { Gc(raw) }

    unsafe fn as_mut_ptr(&mut self) -> *mut T { self.0 as _ }
}

impl Gc<Object> {
    fn mark(mut self, heap: &mut Heap) -> Gc<Object> {
        self.header.mark();
        self // non-moving GC ATM; returned for forwards compatibility
    }
}

// ---

const TAG_BITCOUNT: u32 = 2;
const TAG_MASK: usize = (1 << TAG_BITCOUNT) - 1;
const BYTES_SHIFT: usize = 1;
const BYTES_BIT: usize = 1 << BYTES_SHIFT;
const MARK_BIT: usize = 0b01;

#[derive (Debug)]
struct Header(usize);

impl Header {
    fn new(granule_size: GSize, is_bytes: bool) -> Header {
        Header(granule_size.0 << TAG_BITCOUNT | (is_bytes as usize) << BYTES_SHIFT)
    }

    fn filler(granule_size: GSize) -> Header {
        Header(granule_size.0 << TAG_BITCOUNT | BYTES_BIT)
    }

    fn granule_size(&self) -> GSize { GSize(self.0 >> TAG_BITCOUNT) }

    fn set_granule_size(&mut self, size: GSize) {
        self.0 = (size.0 << TAG_BITCOUNT) | (self.0 & TAG_MASK)
    }

    fn is_bytes(&self) -> bool { (self.0 & BYTES_BIT) == BYTES_BIT }

    fn is_marked(&self) -> bool { (self.0 & MARK_BIT) == MARK_BIT }

    fn mark(&mut self) { self.0 = self.0 | MARK_BIT }

    fn unmark(&mut self) { self.0 = self.0 & !MARK_BIT }
}

// ---

#[repr(C)]
struct Object {
    header: Header,
    class: Gc<Object>
}

impl Object {
    fn granule_size(&self) -> GSize { self.header.granule_size() }

    fn set_granule_size(&mut self, size: GSize) { self.header.set_granule_size(size) }

    fn is_marked(&self) -> bool { self.header.is_marked() }

    fn slots_mut(&mut self) -> Option<&mut [Gc<Object>]> {
        if self.header.is_bytes() {
            None
        } else {
            Some(unsafe {
                slice::from_raw_parts_mut(
                    (self as *mut Object).add(1) as *mut Gc<Object>,
                    self.header.granule_size().0
                )
            })
        }
    }

    unsafe fn scan(&mut self, heap: &mut Heap, greys: &mut Vec<Gc<Object>>) {
        self.slots_mut().map(|slots|
            for oref in slots {
                if !oref.is_marked() {
                    *oref = oref.mark(heap);
                    greys.push(*oref);
                }
            }
        );
    }
}

// ---

#[repr(C)]
struct FreeListNode {
    header: Header,
    rest: Option<NonNull<FreeListNode>>
}

impl FreeListNode {
    fn granule_size(&self) -> GSize { self.header.granule_size() }
}

struct FreeList {
    first: Option<NonNull<FreeListNode>>,
    last: Option<NonNull<FreeListNode>>
}

impl FreeList {
    fn new() -> FreeList { FreeList {first: None, last: None} }

    fn singleton(link: NonNull<FreeListNode>) -> FreeList {
        FreeList {first: Some(link), last: Some(link)}
    }

    fn push(&mut self, mut link: NonNull<FreeListNode>) {
        unsafe {
            link.as_mut().rest = None;
            match self.last {
                Some(mut prev) => {
                    prev.as_mut().rest = Some(link);
                    self.last = Some(link);
                },
                None => *self = FreeList::singleton(link)
            }
        }
    }
}

// ---

struct Heap {
    start: *mut Object,
    end: *mut Object,
    free: FreeList
}

impl Heap {
    fn new(max_size: usize) -> Option<Heap> {
        unsafe {
            let max_granule_size = GSize(max_size / size_of::<Granule>());
            let max_size = max_granule_size.in_bytes(); // rounded down, unlike `GSize::in_granules`
            let start = alloc_zeroed(Layout::from_size_align_unchecked(max_size, align_of::<Granule>()));
            let end = start.add(max_size);
            let mut pangaea = NonNull::new(start as *mut FreeListNode)?;
            *(pangaea.as_mut()) = FreeListNode {
                header: Header::filler(max_granule_size),
                rest: None
            };
            Some(Heap {
                start: start as *mut Object,
                end: end as *mut Object,
                free: FreeList {first: Some(pangaea), last: Some(pangaea)}
            })
        }
    }

    // FIXME: alignment
    unsafe fn alloc(&mut self, class: Gc<Object>, size: usize, is_bytes: bool) -> Option<NonNull<Object>> {
        let granule_size = GSize::in_granules(size)?;
        let mut prev: *mut FreeListNode = self.free.first?.as_mut() as _;
        loop {
            let mut curr: *mut FreeListNode = (*prev).rest?.as_mut() as _;
            let curr_size = (*curr).granule_size();
            if curr_size < granule_size {
                prev = curr;
            } else {
                let slop: GSize = curr_size - granule_size;

                if slop.0 == 0 {
                    (*prev).rest = (*curr).rest;
                } else if slop < GSize::of::<FreeListNode>()? {
                    let hole_header: *mut Header = (curr as *const Granule).add(granule_size.0) as _;
                    *hole_header = Header::filler(slop);
                    (*prev).rest = (*curr).rest;
                } else {
                    let remainder: *mut FreeListNode = (curr as *const Granule).add(granule_size.0) as _;
                    *remainder = FreeListNode {
                        header: Header::filler(slop),
                        rest: (*curr).rest
                    };
                    (*prev).rest = Some(NonNull::new_unchecked(remainder));
                }

                let obj: *mut Object = curr as _;
                *obj = Object {
                    header: Header::new(granule_size, is_bytes),
                    class: class
                };
                return Some(NonNull::new_unchecked(obj));
            }
        }
    }

    unsafe fn gc(&mut self, roots: &mut [Gc<Object>]) {
        self.mark(roots);
        self.sweep();
    }

    unsafe fn mark(&mut self, roots: &mut [Gc<Object>]) {
        let mut greys: Vec<Gc<Object>> = Vec::new(); // Explicit mark stack to avoid stack overflow

        for root in roots {
            if !root.is_marked() {
                *root = root.mark(self);
                greys.push(*root);
            }
        }

        while let Some(mut obj) = greys.pop() {
            obj.scan(self, &mut greys);
        }
    }

    // FIXME: zeroing
    unsafe fn sweep(&mut self) {
        self.free = FreeList::new();

        let mut objs = self.headers();
        while let Some(mut header_ptr) = objs.next() {
            let header: &mut Header = header_ptr.as_mut();
            if header.is_marked() {
                header.unmark();
            } else {
                let mut granule_size = header.granule_size();
                while let Some(mut next) = objs.next() {
                    let next: &mut Header = next.as_mut();
                    if next.is_marked() {
                        next.unmark();
                        break;
                    } else {
                        granule_size += (*next).granule_size();
                    }
                }

                header.set_granule_size(granule_size);
                if granule_size > GSize::of::<Object>().unwrap() {
                    let link: *mut FreeListNode = transmute(header);
                    self.free.push(NonNull::new_unchecked(link));
                }
            }
        }
    }

    fn headers(&mut self) -> Headers {
        Headers {scan: self.start as _, end: self.end as _}
    }
}

struct Headers {
    scan: *mut Header,
    end: *mut Header
}

impl Iterator for Headers {
    type Item = NonNull<Header>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.scan < self.end {
            unsafe {
                let obj = NonNull::new_unchecked(self.scan);
                self.scan = self.scan.add((*self.scan).granule_size().0);
                Some(obj)
            }
        } else {
            None
        }
    }
}

