use std::alloc::{Layout, alloc_zeroed, dealloc};
use std::convert::TryFrom;
use std::mem::{swap, size_of, align_of};
use std::ptr;
use std::slice;

use super::orefs::{Gc, ORef};
use super::object::Object;
use super::granule::{Granule, GSize};

// TODO: Copying GC, to prove that `Root` really works safely (or not...)
// TODO: Python None = ptr::null?

impl<T> Gc<T> {
    pub unsafe fn mark(mut self, heap: &mut Heap) -> Gc<T> {
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
    unsafe fn mark(self, heap: &mut Heap) -> ORef {
        if let Ok(obj) = Gc::try_from(self) {
            obj.mark(heap).into()
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
    fn new(max_size: usize) -> Option<Self> {
        unsafe {
            let max_gsize = GSize::from(max_size / size_of::<Granule>());
            let max_size = max_gsize.in_bytes(); // rounded down, unlike `GSize::in_granules`
            let start = alloc_zeroed(Layout::from_size_align_unchecked(max_size, align_of::<Granule>()));
            if !start.is_null() {
                Some(Self {
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

pub struct Heap {
    max_size: usize,
    fromspace: Semispace,
    tospace: Semispace,
    free_slots: *mut u8,
    latest_bytes: *mut u8,
    grey: *mut u8
}

impl Heap {
    pub fn new(initial_size: usize, max_size: usize) -> Option<Heap> {
        debug_assert!(initial_size <= max_size);

        let max_gsize = GSize::from(max_size / size_of::<Granule>());
        let max_size = max_gsize.in_bytes(); // rounded down, unlike `GSize::in_granules`
        let fromspace = Semispace::new(initial_size / 2)?;
        let tospace = Semispace::new(initial_size / 2)?;
        let free_slots = tospace.start;
        let latest_bytes = tospace.end;
        Some(Self {max_size, fromspace, tospace, free_slots, latest_bytes, grey: ptr::null_mut()})
    }

    pub unsafe fn alloc_slots(&mut self, class: ORef, len: usize) -> Option<Gc<Object>> {
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

    pub unsafe fn alloc_bytes(&mut self, class: ORef, size: usize, align: usize) -> Option<Gc<Object>> {
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
    pub unsafe fn prepare_gc(&mut self) {
        swap(&mut self.fromspace, &mut self.tospace);
        self.free_slots = self.tospace.start;
        self.latest_bytes = self.tospace.end;
        self.grey = self.free_slots;
    }

    pub unsafe fn mark_root(&mut self, root: ORef) -> ORef { root.mark(self) }

    pub unsafe fn gc(&mut self) {
        while self.grey < self.free_slots {
            let heading: *mut Heading = self.grey as _;
            let len = (*heading).raw_size() - usize::from(GSize::of::<Header>());

            let class: *mut ORef = heading.add(1) as _;
            *class = (*class).mark(self);

            let mut slot = class.add(1);
            for _ in 0..len {
                *slot = (*slot).mark(self);
                slot = slot.add(1);
            }

            self.grey = slot as *mut u8;
        }
    }
}

// ---

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
    }

    #[test]
    fn new_sheap() {
        let heap = Heap::new(1 << 22 /* 4 MiB */, (1 << 22) + 3).unwrap();
        assert_eq!(heap.max_size, 1 << 22);
        assert_eq!(heap.fromspace.size(), 1 << 21);
        assert_eq!(heap.tospace.size(), 1 << 21);
        assert!(heap.free_slots != ptr::null_mut());
        assert!(heap.latest_bytes != ptr::null_mut());
        assert_eq!(heap.grey, ptr::null_mut());
    }

    #[test]
    fn salloc_slots() {
        let mut heap = Heap::new(1 << 22, 1 << 22).unwrap();
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
    fn salloc_bytes() {
        let mut heap = Heap::new(1 << 22, 1 << 22).unwrap();
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
    fn scollect() {
        let mut heap = Heap::new(1000, 1000).unwrap();
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
                *root = root.mark(&mut heap);
            }

            heap.gc();
        }

        assert!((heap.free_slots as usize) > heap.tospace.start as usize);
        assert!((heap.free_slots as usize) < heap.latest_bytes as usize);
        assert!((heap.latest_bytes as usize) < heap.tospace.end as usize);
        assert_eq!(heap.grey, heap.free_slots);
    }
}

