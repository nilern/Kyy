use intrusive_collections::{intrusive_adapter, SinglyLinkedListLink, UnsafeRef, SinglyLinkedList};
use intrusive_collections::singly_linked_list;
use std::alloc::{Layout, alloc_zeroed, dealloc};
use std::convert::TryFrom;
use std::mem::{transmute, size_of, align_of};
use std::ops::{Add, AddAssign, Sub};
use std::ptr::{self, NonNull};
use std::slice;

// TODO: Python None = ptr::null?

struct Granule(usize);

// ---

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct GSize(usize);

impl From<usize> for GSize {
    fn from(n: usize) -> GSize { GSize(n) }
}

impl From<GSize> for usize {
    fn from(n: GSize) -> usize { n.0 }
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

    pub fn of<T>() -> GSize {
        GSize((size_of::<T>() + size_of::<Granule>() - 1) / size_of::<Granule>())
    }

    fn in_bytes(self) -> usize { self.0 * size_of::<Granule>() }
}

// ---

#[derive(Debug, Eq)]
pub struct Gc<T>(NonNull<T>);

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self { *self }
}

impl<T> Copy for Gc<T> {}

impl<T> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool { self.0 == other.0 }
}

impl<T> Gc<T> {
    pub unsafe fn from_raw(raw: *mut T) -> Gc<T> { Gc(NonNull::new_unchecked(raw)) }

    pub unsafe fn unchecked_cast<U>(self) -> Gc<U> { Gc(self.0.cast()) }

    /// Safety: The returned reference must not be live across a safepoint.
    pub unsafe fn as_ref(&self) -> &T { self.0.as_ref() }

    pub unsafe fn as_mut_ptr(&mut self) -> *mut T { self.0.as_mut() as _ }

    pub fn header<'a>(&'a self) -> &'a Header {
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

    pub unsafe fn mark(mut self, heap: &mut Heap) -> Gc<T> {
        if !self.is_marked() {
            self.header_mut().mark();
            heap.greys.push(Gc(self.0.cast()));
        }
        self // non-moving GC ATM; returned for forwards compatibility
    }

    pub fn set_class(&mut self, class: ORef) { self.header_mut().class = class }
}

// ---

// Can be null, unlike Gc<T>
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ORef(*mut ());

impl ORef {
    pub const NULL: ORef = ORef(ptr::null_mut());

    fn is_null(self) -> bool { self.0 == ptr::null_mut() }

    pub unsafe fn unchecked_cast<T>(self) -> Gc<T> { Gc::from_raw(self.0 as *mut T) }

    unsafe fn mark(self, heap: &mut Heap) -> ORef {
        if let Ok(obj) = Gc::try_from(self) {
            obj.mark(heap).into()
        } else {
            self
        }
    }
}

impl TryFrom<ORef> for Gc<()> {
    type Error = ();

    fn try_from(oref: ORef) -> Result<Gc<()>, ()> {
        if !oref.is_null() { unsafe{ Ok(oref.unchecked_cast()) } } else { Err(()) }
    }
}

impl<T> From<Gc<T>> for ORef {
    fn from(mut obj: Gc<T>) -> ORef { unsafe { ORef(obj.0.as_mut() as *mut T as _) } }
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

    fn filler(size: usize) -> Heading { Heading(size << TAG_BITCOUNT | BYTES_BIT) }

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
            GSize::from(self.0 >> TAG_BITCOUNT).in_bytes()
        }
    }

    fn is_marked(&self) -> bool { (self.0 & MARK_BIT) == MARK_BIT }

    fn mark(&mut self) { self.0 = self.0 | MARK_BIT }

    fn unmark(&mut self) { self.0 = self.0 & !MARK_BIT }
}

// ---

#[repr(C)]
pub struct Header {
    heading: Heading,
    class: ORef
}

impl Header {
    pub fn gsize(&self) -> GSize { self.heading.gsize() }

    pub fn size(&self) -> usize { self.heading.size() }

    fn is_bytes(&self) -> bool { self.heading.is_bytes() }

    fn mark(&mut self) { self.heading.mark(); }

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
    heading: usize,
    link: SinglyLinkedListLink
}

intrusive_adapter!(FreeListAdapter = UnsafeRef<FreeListNode>: FreeListNode { link: SinglyLinkedListLink });

type FreeList = SinglyLinkedList<FreeListAdapter>;

impl FreeListNode {
    fn new(size: usize) -> FreeListNode {
        FreeListNode { heading: size << TAG_BITCOUNT | BYTES_BIT, link: SinglyLinkedListLink::new() }
    }

    fn size(&self) -> usize { self.heading >> TAG_BITCOUNT }

    fn set_size(&mut self, size: usize) { self.heading = size << TAG_BITCOUNT | BYTES_BIT }
}

// ---

pub struct Heap {
    start: *mut Header,
    end: *mut Header,
    free: FreeList,
    greys: Vec<Gc<()>>
}

impl Drop for Heap {
    fn drop(&mut self) {
        self.free.clear();
        unsafe {
            dealloc(self.start as _, Layout::from_size_align_unchecked(self.size(), align_of::<Granule>()));
        }
    }
}

impl Heap {
    pub fn new(max_size: usize) -> Option<Heap> {
        unsafe {
            let max_gsize = GSize(max_size / size_of::<Granule>());
            let max_size = max_gsize.in_bytes(); // rounded down, unlike `GSize::in_granules`
            let start = alloc_zeroed(Layout::from_size_align_unchecked(max_size, align_of::<Granule>()));
            if !start.is_null() {
                let end = start.add(max_size);
                let pangaea = start as *mut FreeListNode;
                *pangaea = FreeListNode::new(max_size);
                let mut free = SinglyLinkedList::new(FreeListAdapter::new());
                free.push_front(UnsafeRef::from_raw(pangaea));
                Some(Heap {
                    start: start as *mut Header,
                    end: end as *mut Header,
                    free: free,
                    greys: Vec::new()
                })
            } else {
                None
            }
        }
    }

    fn size(&self) -> usize { self.end as usize - self.start as usize }

    unsafe fn alloc(&mut self, heading: Heading, class: ORef, size: usize, align: usize) -> Option<Gc<()>> {
        debug_assert!(heading.size() == size_of::<Header>() + size);
        debug_assert!(align.is_power_of_two());
        debug_assert!(heading.is_bytes() || align == align_of::<Granule>());

        let align = align.max(align_of::<Header>());
        let mut prev: singly_linked_list::CursorMut<FreeListAdapter> = self.free.cursor_mut();

        loop {
            let curr: *mut FreeListNode = transmute::<&FreeListNode, _>(prev.peek_next().get()?);
            let start_addr: usize = curr as _;
            let end_addr = start_addr + (*curr).size();

            if let Some(obj_addr) = end_addr.checked_sub(size) {
                let obj_addr = obj_addr & !(align - 1);

                if let Some(header_addr) = obj_addr.checked_sub(size_of::<Header>()) {
                    if let Some(slop) = header_addr.checked_sub(start_addr) {
                        if slop == 0 {
                            prev.remove_next();
                        } else if slop < size_of::<FreeListNode>() {
                            prev.remove_next();
                            // Because of alignment requirements, here `slop == size_of::<Granule>()`:
                            *(curr as *mut Heading) = Heading::filler(slop);
                        } else {
                            (*curr).set_size(slop);
                        }

                        *(header_addr as *mut Header) = Header {heading, class};
                        return Some(Gc::from_raw(obj_addr as *mut ()));
                    }
                }
            }

            prev.move_next();
        }
    }

    pub unsafe fn alloc_slots(&mut self, class: ORef, len: usize) -> Option<Gc<()>> {
        let gsize = GSize::from(len);
        self.alloc(Heading::slots(gsize), class, gsize.in_bytes(), align_of::<Granule>())
    }

    pub unsafe fn alloc_bytes(&mut self, class: ORef, len: usize, align: usize) -> Option<Gc<()>> {
        self.alloc(Heading::bytes(len), class, len, align)
    }

    pub unsafe fn mark_root(&mut self, root: ORef) -> ORef { root.mark(self) }

    pub unsafe fn gc(&mut self) {
        self.mark();
        self.sweep();
    }

    unsafe fn mark(&mut self) {
        while let Some(mut obj) = self.greys.pop() {
            obj.header_mut().slots_mut().map(|slots|
                for slot in slots {
                    *slot = slot.mark(self);
                }
            );
        }

        self.greys.shrink_to_fit();
    }

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

                let size = gsize.in_bytes();
                if size < size_of::<FreeListNode>() {
                    *heading = Heading::filler(size);
                } else {
                    let heading: *mut Heading = heading as _;
                    let node: *mut FreeListNode = heading as _;
                    *node = FreeListNode::new(size);
                    ptr::write_bytes(node.add(1) as *mut u8, 0, size - size_of::<FreeListNode>());

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
        assert_eq!(heap.size(), 1 << 22);
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

    #[test]
    fn collect() {
        let mut heap = Heap::new(1000).unwrap();
        let heap_size = heap.end as usize - heap.start as usize;

        let mut size = 0;
        loop {
            if size % 3 == 0 {
                if let Some(obj) = unsafe { heap.alloc_bytes(ORef::NULL, size, align_of::<f64>()) } {
                    assert_eq!(obj.header().size(), size_of::<Header>() + size);
                    assert!(obj.header().is_bytes());
                    assert!(!obj.header().is_marked());
                } else {
                    break;
                }
            } else {
                let len = GSize::in_granules(size).unwrap().0;
                if let Some(obj) = unsafe { heap.alloc_slots(ORef::NULL, len) } {
                    assert_eq!(obj.header().gsize(), GSize::of::<Header>() + GSize::from(len));
                    assert!(!obj.header().is_bytes());
                    assert!(!obj.header().is_marked());
                } else {
                    break;
                }
            }

            size += 1;
        }

        unsafe { heap.gc(); }
        assert_eq!(heap.free.cursor().peek_next().get().unwrap().size(), heap_size);
    }
}

