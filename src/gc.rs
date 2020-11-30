use intrusive_collections::{intrusive_adapter, SinglyLinkedListLink, UnsafeRef, SinglyLinkedList};
use intrusive_collections::singly_linked_list;
use std::alloc::{Layout, alloc_zeroed};
use std::convert::TryFrom;
use std::mem::{transmute, size_of, align_of};
use std::ops::{Deref, DerefMut, Add, AddAssign, Sub};
use std::ptr::NonNull;
use std::slice;

// TODO: Python None = ptr::null?

struct Granule(usize);

// ---

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct GSize(usize);

impl From<usize> for GSize {
    fn from(n: usize) -> GSize { GSize(n) }
}

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
        Some(GSize(size.checked_add(size_of::<Granule>() - 1)? / size_of::<Granule>()))
    }

    fn of<T>() -> GSize {
        GSize((size_of::<T>() + size_of::<Granule>() - 1) / size_of::<Granule>())
    }

    fn in_bytes(self) -> usize { self.0 * size_of::<Granule>() }
}

// ---

#[derive(Debug, Eq)]
struct Gc<T>(NonNull<T>);

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
    unsafe fn from_raw(raw: *mut T) -> Gc<T> { Gc(NonNull::new_unchecked(raw)) }

    unsafe fn as_mut_ptr(&mut self) -> *mut T { self.0.as_mut() as _ }

    fn header<'a>(&'a self) -> &'a Header {
        unsafe {
            let ptr = self.0.as_ref() as *const T;
            transmute((ptr as *const Header).offset(-1))
        }
    }

    fn header_mut<'a>(&'a mut self) -> &'a mut Header {
        unsafe {
            let ptr = self.0.as_mut() as *mut T;
            transmute((ptr as *mut Header).offset(-1))
        }
    }

    fn is_marked(self) -> bool { self.header().is_marked() }

    fn mark(mut self, heap: &mut Heap) -> Gc<T> {
        self.header_mut().mark();
        self // non-moving GC ATM; returned for forwards compatibility
    }
}

// ---

// Can be null and cannot be deref'd, unlike Gc<T>
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ORef(usize);

impl ORef {
    const NULL: ORef = ORef(0);

    fn is_null(self) -> bool { self.0 == 0 }

    unsafe fn unchecked_downcast<T>(self) -> Gc<T> { Gc::from_raw(self.0 as *mut T) }

    fn as_ptr(self) -> Option<NonNull<()>> { NonNull::new(self.0 as *mut ()) }
}

impl TryFrom<ORef> for Gc<()> {
    type Error = ();

    fn try_from(oref: ORef) -> Result<Gc<()>, ()> {
        if !oref.is_null() { unsafe{ Ok(oref.unchecked_downcast()) } } else { Err(()) }
    }
}

impl<T> From<Gc<T>> for ORef {
    fn from(obj: Gc<T>) -> ORef { unsafe { ORef(transmute(obj.0.as_ref())) } }
}

// ---

const TAG_BITCOUNT: u32 = 2;
const TAG_MASK: usize = (1 << TAG_BITCOUNT) - 1;
const BYTES_SHIFT: usize = 1;
const BYTES_BIT: usize = 1 << BYTES_SHIFT;
const MARK_BIT: usize = 0b01;

#[derive(Debug)]
struct Heading(usize);

impl Heading {
    fn bytes(size: usize) -> Heading {
        let size = size + size_of::<Header>();
        Heading(size << TAG_BITCOUNT | BYTES_BIT)
    }

    fn slots(gsize: GSize) -> Heading {
        let gsize = gsize + GSize::of::<Header>();
        Heading(gsize.0 << TAG_BITCOUNT)
    }

    fn filler(gsize: GSize) -> Heading { Heading::slots(gsize) }

    fn is_bytes(&self) -> bool { (self.0 & BYTES_BIT) == BYTES_BIT }

    // OPTIMIZE: Don't include Header size (requires alignment hole recognizability).
    fn gsize(&self) -> GSize {
        if self.is_bytes() {
            GSize::in_granules(self.0 >> TAG_BITCOUNT).unwrap()
        } else {
            GSize::from(self.0 >> TAG_BITCOUNT)
        }
    }

    fn size(&self) -> usize {
        if self.is_bytes() {
            self.0 >> TAG_BITCOUNT
        } else {
            (self.0 >> TAG_BITCOUNT) * size_of::<Granule>()
        }
    }

    fn is_marked(&self) -> bool { (self.0 & MARK_BIT) == MARK_BIT }

    fn mark(&mut self) { self.0 = self.0 | MARK_BIT }

    fn unmark(&mut self) { self.0 = self.0 & !MARK_BIT }
}

// ---

#[repr(C)]
struct Header {
    heading: Heading,
    class: ORef
}

impl Header {
    fn gsize(&self) -> GSize { self.heading.gsize() }

    fn size(&self) -> usize { self.heading.size() }

    fn is_bytes(&self) -> bool { self.heading.is_bytes() }

    fn mark(&mut self) { self.heading.mark(); }

    fn unmark(&mut self) { self.heading.unmark(); }

    fn is_marked(&self) -> bool { self.heading.is_marked() }

    fn slots_mut(&mut self) -> Option<&mut [ORef]> {
        if self.heading.is_bytes() {
            None
        } else {
            Some(unsafe {
                slice::from_raw_parts_mut(
                    (self as *mut Header).add(1) as *mut ORef,
                    self.heading.gsize().0
                )
            })
        }
    }
}

// ---

#[repr(C)]
struct FreeListNode {
    heading: Heading,
    link: SinglyLinkedListLink
}

intrusive_adapter!(FreeListAdapter = UnsafeRef<FreeListNode>: FreeListNode { link: SinglyLinkedListLink });

type FreeList = SinglyLinkedList<FreeListAdapter>;

impl FreeListNode {
    fn gsize(&self) -> GSize { self.heading.gsize() }
}

// ---

struct Heap {
    start: *mut Header,
    end: *mut Header,
    free: FreeList
}

impl Heap {
    fn new(max_size: usize) -> Option<Heap> {
        unsafe {
            let max_gsize = GSize(max_size / size_of::<Granule>());
            let max_size = max_gsize.in_bytes(); // rounded down, unlike `GSize::in_granules`
            let start = alloc_zeroed(Layout::from_size_align_unchecked(max_size, align_of::<Granule>()));
            if !start.is_null() {
                let end = start.add(max_size);
                let pangaea = start as *mut FreeListNode;
                *pangaea = FreeListNode {
                    heading: Heading::filler(max_gsize),
                    link: SinglyLinkedListLink::new()
                };
                let mut free = SinglyLinkedList::new(FreeListAdapter::new());
                free.push_front(UnsafeRef::from_raw(pangaea));
                Some(Heap {
                    start: start as *mut Header,
                    end: end as *mut Header,
                    free: free
                })
            } else {
                None
            }
        }
    }

    // FIXME: alignment
    unsafe fn alloc(&mut self, heading: Heading, class: ORef, align: usize) -> Option<Gc<()>> {
        let gsize = heading.gsize();
        let mut prev: singly_linked_list::CursorMut<FreeListAdapter> = self.free.cursor_mut();

        loop {
            let curr_size = prev.peek_next().get()?.gsize();
            if curr_size < gsize {
                prev.move_next();
            } else {
                let obj: *mut Header = UnsafeRef::into_raw(prev.remove_next()?) as _;

                let slop: GSize = curr_size - gsize;
                if slop > GSize::from(0) {
                    let hole: *mut Heading = (obj as *const Granule).add(gsize.0) as _;
                    *hole = Heading::filler(slop);
                    if slop >= GSize::of::<FreeListNode>() {
                        let remainder: *mut FreeListNode = hole as _;
                        (*remainder).link = SinglyLinkedListLink::new();
                        prev.insert_after(UnsafeRef::from_raw(remainder));
                    }
                }

                *obj = Header { heading, class };
                return Some(Gc::from_raw(obj.add(1) as *mut ()));
            }
        }
    }

    unsafe fn alloc_slots(&mut self, class: ORef, len: usize) -> Option<Gc<()>> {
        self.alloc(Heading::slots(GSize::from(len)), class, align_of::<Granule>())
    }

    unsafe fn alloc_bytes(&mut self, class: ORef, len: usize, align: usize) -> Option<Gc<()>> {
        self.alloc(Heading::bytes(len), class, align_of::<u8>())
    }

    unsafe fn gc(&mut self, roots: &mut [ORef]) {
        self.mark(roots);
        self.sweep();
    }

    unsafe fn mark(&mut self, roots: &mut [ORef]) {
        let mut greys: Vec<Gc<()>> = Vec::new(); // Explicit mark stack to avoid stack overflow

        for root in roots {
            if let Ok(obj) = Gc::try_from(*root) {
                if !obj.is_marked() {
                    *root = ORef::from(obj.mark(self));
                    greys.push(obj);
                }
            }
        }

        while let Some(mut obj) = greys.pop() {
            obj.header_mut().slots_mut().map(|slots|
                for slot in slots {
                    if let Ok(obj) = Gc::try_from(*slot) {
                        if !obj.is_marked() {
                            *slot = ORef::from(obj.mark(self));
                            greys.push(obj);
                        }
                    }
                }
            );
        }
    }

    // FIXME: zeroing
    unsafe fn sweep(&mut self) {
        self.free.fast_clear();
        let mut free = self.free.cursor_mut();

        let mut objs = Headings {scan: self.start as _, end: self.end as _};
        while let Some(mut heading) = objs.next() {
            let heading: &mut Heading = heading.as_mut();
            if heading.is_marked() {
                heading.unmark();
            } else {
                let mut gsize = heading.gsize();
                while let Some(mut next) = objs.next() {
                    let next: &mut Heading = next.as_mut();
                    if next.is_marked() {
                        next.unmark();
                        break;
                    } else {
                        gsize += (*next).gsize();
                    }
                }

                *heading = Heading::filler(gsize);

                if gsize >= GSize::of::<Header>() {
                    let node: *mut FreeListNode = transmute(heading);
                    (*node).link = SinglyLinkedListLink::new();
                    free.insert_after(UnsafeRef::from_raw(node));
                    free.move_next();
                }
            }
        }
    }
}

struct Headings {
    scan: *mut Heading,
    end: *mut Heading
}

impl Iterator for Headings {
    type Item = NonNull<Heading>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.scan < self.end {
            unsafe {
                let obj = NonNull::new_unchecked(self.scan);
                self.scan = self.scan.add((*self.scan).gsize().0);
                Some(obj)
            }
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    use std::ptr;

    #[test]
    fn sizes() {
        assert_eq!(size_of::<Granule>(), size_of::<usize>()); // granule is "word"-sized

        // Object references are granule-sized:
        assert_eq!(size_of::<Gc<()>>(), size_of::<Granule>());
        assert_eq!(size_of::<ORef>(), size_of::<Granule>());

        // Gc<T> has null-pointer optimization:
        assert_eq!(size_of::<Option<Gc<()>>>(), size_of::<Granule>());

        // Header/free list node is two granules:
        assert_eq!(size_of::<Header>(), 2*size_of::<Granule>());
        assert_eq!(size_of::<FreeListNode>(), size_of::<Header>());
    }

    #[test]
    fn new_heap() {
        let heap = Heap::new((1 << 22 /* 4 MiB */) + 3).unwrap();
        assert!(heap.start != ptr::null_mut());
        assert_eq!(heap.end as usize - heap.start as usize, 1 << 22);
    }

    #[test]
    fn alloc_slots() {
        let mut heap = Heap::new(1 << 22).unwrap();
        let len = 1 << 7;
        let obj: Gc<()> = unsafe { heap.alloc_slots(ORef::NULL, len).unwrap() };
        let header = obj.header();
        assert_eq!(header.gsize(), GSize::of::<Header>() + GSize(len));
        assert_eq!(header.size(), size_of::<Header>() + size_of::<Granule>() * len);
        assert!(!header.is_bytes());
        assert!(!header.is_marked());
        assert_eq!(header.class, ORef::NULL);
    }

    #[test]
    fn alloc_bytes() {
        let mut heap = Heap::new(1 << 22).unwrap();
        let len = (1 << 10) + 3;
        let obj: Gc<()> = unsafe { heap.alloc_bytes(ORef::NULL, len, align_of::<u8>()).unwrap() };
        let header = obj.header();
        assert_eq!(header.gsize(), GSize::of::<Header>() + GSize::in_granules(len).unwrap());
        assert_eq!(header.size(), size_of::<Header>() + len);
        assert!(header.is_bytes());
        assert!(!header.is_marked());
        assert_eq!(header.class, ORef::NULL);
    }
}

