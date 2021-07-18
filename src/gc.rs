use intrusive_collections::{intrusive_adapter, SinglyLinkedListLink, UnsafeRef, SinglyLinkedList};
use intrusive_collections::singly_linked_list;
use std::alloc::{Layout, alloc_zeroed, dealloc};
use std::convert::TryFrom;
use std::mem::{transmute, swap, size_of, align_of};
use std::ptr::{self, NonNull};
use std::slice;

use super::orefs::{Gc, ORef};
use super::object::Object;
use super::granule::{Granule, GSize};

// TODO: Copying GC, to prove that `Root` really works safely (or not...)
// TODO: Python None = ptr::null?

impl<T> Gc<T> {
    pub unsafe fn mark(mut self, heap: &mut Heap) -> Gc<T> {
        if !self.is_marked() {
            self.header_mut().mark();
            heap.greys.push(self.as_obj());
        }
        self // non-moving GC ATM; returned for forwards compatibility
    }

    unsafe fn semis_mark(mut self, heap: &mut SemisHeap) -> Gc<T> {
        match self.header().forwarded() {
            Some(res) => res,
            None => {
                let res = heap.alloc_clone(self).unwrap();
                self.header_mut().forward(res);
                res
            }
        }
    }
}

impl ORef {
    unsafe fn semis_mark(self, heap: &mut SemisHeap) -> ORef {
        if let Ok(obj) = Gc::try_from(self) {
            obj.semis_mark(heap).into()
        } else {
            self
        }
    }
}


// ---

const TAG_BITCOUNT: u32 = 2;
const TAG_MASK: usize = (1 << TAG_BITCOUNT) - 1;
const BYTES_SHIFT: usize = 1;
const BYTES_BIT: usize = 1 << BYTES_SHIFT;
const MARK_BIT: usize = 0b01;

/// (62 | 30) bits of size (in granules for slots objects and in bytes for bytes objects)
/// 1 bit for `is_bytes` flag
/// 1 bit for `is_marked` flag
#[derive(Debug)]
struct Heading(usize);

impl Heading {
    fn bytes(size: usize) -> Heading {
        let size = size + size_of::<Header>();
        Heading(size << TAG_BITCOUNT | BYTES_BIT)
    }

    fn slots(gsize: GSize) -> Heading {
        let gsize = gsize + GSize::of::<Header>();
        Heading(usize::from(gsize) << TAG_BITCOUNT)
    }

    fn filler(size: usize) -> Heading { Heading(size << TAG_BITCOUNT | BYTES_BIT) }

    fn is_bytes(&self) -> bool { (self.0 & BYTES_BIT) == BYTES_BIT }

    fn raw_size(&self) -> usize { self.0 >> TAG_BITCOUNT }

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

    pub fn mark(&mut self) { self.heading.mark(); }

    pub fn is_marked(&self) -> bool { self.heading.is_marked() }

    pub fn class(&self) -> ORef { self.class }

    pub fn class_mut(&mut self) -> &mut ORef { &mut self.class }

    fn slots_mut(&mut self) -> Option<&mut [ORef]> {
        if self.heading.is_bytes() {
            None
        } else {
            Some(unsafe {
                slice::from_raw_parts_mut(
                    (self as *mut Header).add(1) as *mut ORef,
                    self.heading.gsize().into()
                )
            })
        }
    }

    unsafe fn forwarded<T>(&self) -> Option<Gc<T>> {
        if self.heading.is_marked() {
            Some(self.class.unchecked_cast())
        } else {
            None
        }
    }

    fn forward<T>(&mut self, dest: Gc<T>) {
        self.heading.mark();
        self.class = dest.into();
    }
}

// ---

struct Semispace {
    start: *mut u8,
    end: *mut u8
}

impl Drop for Semispace {
    fn drop(&mut self) {
        unsafe {
            dealloc(self.start as _, Layout::from_size_align_unchecked(self.size(), align_of::<Granule>()));
        }
    }
}

impl Semispace {
    fn new(max_size: usize) -> Option<Semispace> {
        unsafe {
            let max_gsize = GSize::from(max_size / size_of::<Granule>());
            let max_size = max_gsize.in_bytes(); // rounded down, unlike `GSize::in_granules`
            let start = alloc_zeroed(Layout::from_size_align_unchecked(max_size, align_of::<Granule>()));
            if !start.is_null() {
                Some(Semispace {
                    start,
                    end: start.add(max_size)
                })
            } else {
                None
            }
        }
    }

    fn size(&self) -> usize { self.end as usize - self.start as usize }
}

// ---

struct SemisHeap {
    max_size: usize,
    fromspace: Semispace,
    tospace: Semispace,
    free_slots: *mut u8,
    latest_bytes: *mut u8,
    grey: *mut u8
}

impl SemisHeap {
    fn new(initial_size: usize, max_size: usize) -> Option<SemisHeap> {
        debug_assert!(initial_size <= max_size);

        let max_gsize = GSize::from(max_size / size_of::<Granule>());
        let max_size = max_gsize.in_bytes(); // rounded down, unlike `GSize::in_granules`
        let fromspace = Semispace::new(initial_size / 2)?;
        let tospace = Semispace::new(initial_size / 2)?;
        let free_slots = tospace.start;
        let latest_bytes = tospace.end;
        Some(Self {max_size, fromspace, tospace, free_slots, latest_bytes, grey: ptr::null_mut()})
    }

    unsafe fn alloc_slots(&mut self, class: ORef, len: usize) -> Option<Gc<Object>> {
        let gsize = GSize::from(len);

        let header = self.free_slots as *mut Header;
        let start = (header as usize).checked_add(size_of::<Header>())?;
        let free_slots = start.checked_add(gsize.in_bytes())?;

        if free_slots > self.latest_bytes as usize {
            None
        } else {
            header.write(Header {
                heading: Heading::slots(gsize),
                class
            });

            self.free_slots = free_slots as *mut u8;

            Some(Gc::from_raw(start as *mut Object))
        }
    }

    unsafe fn alloc_bytes(&mut self, class: ORef, size: usize, align: usize) -> Option<Gc<Object>> {
        debug_assert!(align.is_power_of_two());

        let align = align.max(align_of::<Header>());

        let latest_bytes = self.latest_bytes as usize;
        let start = latest_bytes.checked_sub(size)?;
        let start = start & !(align - 1);
        let header = start.checked_sub(size_of::<Header>())?;

        if header < self.free_slots as usize {
            None
        } else {
            (header as *mut Header).write(Header {
                heading: Heading::bytes(size),
                class
            });

            self.latest_bytes = header as *mut u8;

            Some(Gc::from_raw(start as *mut Object))
        }
    }

    fn alloc_clone<T>(&mut self, mut obj: Gc<T>) -> Option<Gc<T>> {
        unsafe {
            let (mut new_obj, size) = if obj.header().is_bytes() {
                let size = obj.header().size() - size_of::<Header>();
                (self.alloc_bytes(obj.class(), size, /* FIXME: */ size_of::<Header>())?, size)
            } else {
                let len = obj.header().gsize() - GSize::of::<Header>();
                (self.alloc_slots(obj.class(), len.into())?, len.in_bytes())
            };

            ptr::copy_nonoverlapping(obj.as_mut_ptr() as *mut u8, new_obj.as_mut_ptr() as *mut u8, size);
            Some(new_obj.unchecked_cast())
        }
    }

    // TODO: Heap expansion logic:
    // FIXME: zero tospace before swap (or after collection):
    unsafe fn prepare_gc(&mut self) {
        swap(&mut self.fromspace, &mut self.tospace);
        self.free_slots = self.tospace.start;
        self.latest_bytes = self.tospace.end;
        self.grey = self.free_slots;
    }

    unsafe fn mark_root(&mut self, root: ORef) -> ORef { root.semis_mark(self) }

    unsafe fn gc(&mut self) {
        while self.grey < self.free_slots {
            let heading: *mut Heading = self.grey as _;
            let len = (*heading).raw_size() - usize::from(GSize::of::<Header>());

            let class: *mut ORef = heading.add(1) as _;
            *class = (*class).semis_mark(self);

            let mut slot = class.add(1);
            for i in 0..len {
                *slot = (*slot).semis_mark(self);
                slot = slot.add(1);
            }

            self.grey = slot as *mut u8;
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
    greys: Vec<Gc<Object>>
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
            let max_gsize = GSize::from(max_size / size_of::<Granule>());
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

    unsafe fn alloc(&mut self, heading: Heading, class: ORef, size: usize, align: usize) -> Option<Gc<Object>> {
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
                        return Some(Gc::from_raw(obj_addr as *mut Object));
                    }
                }
            }

            prev.move_next();
        }
    }

    pub unsafe fn alloc_slots(&mut self, class: ORef, len: usize) -> Option<Gc<Object>> {
        let gsize = GSize::from(len);
        self.alloc(Heading::slots(gsize), class, gsize.in_bytes(), align_of::<Granule>())
    }

    pub unsafe fn alloc_bytes(&mut self, class: ORef, len: usize, align: usize) -> Option<Gc<Object>> {
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
                self.scan = self.scan.add((*self.scan).gsize().into());
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
        assert_eq!(size_of::<Gc<Object>>(), size_of::<Granule>());
        assert_eq!(size_of::<ORef>(), size_of::<Granule>());

        // Gc<T> has null-pointer optimization:
        assert_eq!(size_of::<Option<Gc<Object>>>(), size_of::<Granule>());

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
    fn new_sheap() {
        let heap = SemisHeap::new(1 << 22 /* 4 MiB */, (1 << 22) + 3).unwrap();
        assert_eq!(heap.max_size, 1 << 22);
        assert_eq!(heap.fromspace.size(), 1 << 21);
        assert_eq!(heap.tospace.size(), 1 << 21);
        assert!(heap.free_slots != ptr::null_mut());
        assert!(heap.latest_bytes != ptr::null_mut());
        assert_eq!(heap.grey, ptr::null_mut());
    }

    #[test]
    fn alloc_slots() {
        let mut heap = Heap::new(1 << 22).unwrap();
        let len = 1 << 7;
        let obj: Gc<Object> = unsafe { heap.alloc_slots(ORef::NULL, len).unwrap() };
        let header = unsafe { obj.header() };
        assert_eq!(header.gsize(), GSize::of::<Header>() + GSize::from(len));
        assert_eq!(header.size(), size_of::<Header>() + size_of::<Granule>() * len);
        assert!(!header.is_bytes());
        assert!(!header.is_marked());
        assert_eq!(header.class, ORef::NULL);
    }

    #[test]
    fn salloc_slots() {
        let mut heap = SemisHeap::new(1 << 22, 1 << 22).unwrap();
        let len = 1 << 7;
        let obj: Gc<Object> = unsafe { heap.alloc_slots(ORef::NULL, len).unwrap() };
        let header = unsafe { obj.header() };
        assert_eq!(header.gsize(), GSize::of::<Header>() + GSize::from(len));
        assert_eq!(header.size(), size_of::<Header>() + size_of::<Granule>() * len);
        assert!(!header.is_bytes());
        assert!(!header.is_marked());
        assert_eq!(header.class, ORef::NULL);
    }

    #[test]
    fn alloc_bytes() {
        let mut heap = Heap::new(1 << 22).unwrap();
        let len = (1 << 10) + 3;
        let obj: Gc<Object> = unsafe { heap.alloc_bytes(ORef::NULL, len, align_of::<u8>()).unwrap() };
        let header = unsafe { obj.header() };
        assert_eq!(header.gsize(), GSize::of::<Header>() + GSize::in_granules(len).unwrap());
        assert_eq!(header.size(), size_of::<Header>() + len);
        assert!(header.is_bytes());
        assert!(!header.is_marked());
        assert_eq!(header.class, ORef::NULL);
    }

    #[test]
    fn salloc_bytes() {
        let mut heap = SemisHeap::new(1 << 22, 1 << 22).unwrap();
        let len = (1 << 10) + 3;
        let obj: Gc<Object> = unsafe { heap.alloc_bytes(ORef::NULL, len, align_of::<u8>()).unwrap() };
        let header = unsafe { obj.header() };
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
                    unsafe { 
                        assert_eq!(obj.header().size(), size_of::<Header>() + size);
                        assert!(obj.header().is_bytes());
                        assert!(!obj.header().is_marked());
                    }
                } else {
                    break;
                }
            } else {
                let len = GSize::in_granules(size).unwrap().into();
                if let Some(obj) = unsafe { heap.alloc_slots(ORef::NULL, len) } {
                    unsafe { 
                        assert_eq!(obj.header().gsize(), GSize::of::<Header>() + GSize::from(len));
                        assert!(!obj.header().is_bytes());
                        assert!(!obj.header().is_marked());
                    }
                } else {
                    break;
                }
            }

            size += 1;
        }

        unsafe { heap.gc(); }
        assert_eq!(heap.free.cursor().peek_next().get().unwrap().size(), heap_size);
    }

    #[test]
    fn scollect() {
        let mut heap = SemisHeap::new(1000, 1000).unwrap();
        let mut roots = Vec::new();

        let mut size = 0;
        loop {
            let obj = if size % 3 == 0 {
                if let Some(obj) = unsafe { heap.alloc_bytes(ORef::NULL, size, align_of::<f64>()) } {
                    unsafe { 
                        assert_eq!(obj.header().size(), size_of::<Header>() + size);
                        assert!(obj.header().is_bytes());
                        assert!(!obj.header().is_marked());
                    }

                    obj
                } else {
                    break;
                }
            } else {
                let len = GSize::in_granules(size).unwrap().into();
                if let Some(obj) = unsafe { heap.alloc_slots(ORef::NULL, len) } {
                    unsafe { 
                        assert_eq!(obj.header().gsize(), GSize::of::<Header>() + GSize::from(len));
                        assert!(!obj.header().is_bytes());
                        assert!(!obj.header().is_marked());
                    }

                    obj
                } else {
                    break;
                }
            };

            if size % 2 == 0 {
                roots.push(obj);
            }

            size += 1;
        }

        unsafe {
            heap.prepare_gc();

            for root in roots.iter_mut() {
                *root = root.semis_mark(&mut heap);
            }

            heap.gc();
        }

        assert!((heap.free_slots as usize) > heap.tospace.start as usize);
        assert!((heap.free_slots as usize) < heap.latest_bytes as usize);
        assert!((heap.latest_bytes as usize) < heap.tospace.end as usize);
        assert_eq!(heap.grey, heap.free_slots);
    }
}

