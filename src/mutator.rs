use std::sync::Arc;
use std::cell::UnsafeCell;
use std::mem::{size_of, align_of, transmute};

use super::gc::{self, ORef, Gc};
use super::int::Int;

#[derive(Debug, Clone)]
pub struct Root<T>(Arc<UnsafeCell<Gc<T>>>);

impl<T> Root<T> {
    fn new(oref: Gc<T>) -> Root<T> { Root(Arc::new(UnsafeCell::new(oref))) }

    pub unsafe fn unchecked_downcast<U>(self) -> Root<U> { transmute(self) }

    /// Safety: The returned reference must not be live across a safepoint.
    pub unsafe fn as_ref(&self) -> &T { (*self.0.get()).as_ref() }

    /// Safety: The returned pointer must not be live across a safepoint.
    pub unsafe fn as_mut_ptr(&self) -> *mut T { (*self.0.get()).as_mut_ptr() }

    unsafe fn oref(&self) -> Gc<T> { *self.0.get() }
}

// ---

#[repr(C)]
pub struct Class {}

// ---

pub trait KyyType {
    fn reify(km: &KyyMutator) -> Root<Class>;
}

impl KyyType for Int {
    fn reify(km: &KyyMutator) -> Root<Class> { Root::new(km.int_class) }
}

// ---

pub struct KyyMutator {
    heap: gc::Heap,
    handles: Vec<Root<()>>,
    type_class: Gc<Class>,
    int_class: Gc<Class>
}

impl KyyMutator {
    pub fn new(max_heap_size: usize) -> Option<KyyMutator> {
        let mut heap = gc::Heap::new(max_heap_size)?;
        unsafe {
            let mut type_class: Gc<Class> = heap.alloc_bytes(ORef::NULL, size_of::<Class>(), align_of::<Class>())?
                .unchecked_downcast();
            type_class.set_class(type_class.into());
            let int_class: Gc<Class> = heap.alloc_bytes(type_class.into(), size_of::<Class>(), align_of::<Class>())?
                .unchecked_downcast();
            Some(KyyMutator {
                heap,
                handles: Vec::new(),
                type_class,
                int_class
            })
        }
    }

    pub unsafe fn alloc_slots(&mut self, class: Root<Class>, len: usize) -> Root<()> {
        let oref = self.heap.alloc_slots(class.oref().into(), len).unwrap_or_else(|| {
            self.gc();
            self.heap.alloc_slots(class.oref().into(), len).expect("Kyy out of memory")
        });
        let handle = Root::new(oref);
        self.handles.push(handle.clone());
        handle
    }

    pub unsafe fn alloc_bytes(&mut self, class: Root<Class>, size: usize, align: usize) -> Root<()> {
        let oref = self.heap.alloc_bytes(class.oref().into(), size, align).unwrap_or_else(|| {
            self.gc();
            self.heap.alloc_bytes(class.oref().into(), size, align).expect("Kyy out of memory")
        });
        let handle = Root::new(oref);
        self.handles.push(handle.clone());
        handle
    }

    unsafe fn gc(&mut self) {
        let mut greys: Vec<Gc<()>> = Vec::new(); // explicit mark stack to avoid stack overflow

        self.handles.retain(|handle| Arc::strong_count(&handle.0) > 1);
        for handle in self.handles.iter_mut() {
            *handle.0.get() = self.heap
                .mark_root(&mut greys, (*handle.0.get()).into()).unchecked_downcast();
        }
        self.type_class = self.heap.mark_root(&mut greys, self.type_class.into()).unchecked_downcast();
        self.int_class = self.heap.mark_root(&mut greys, self.int_class.into()).unchecked_downcast();

        self.heap.gc(&mut greys);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::mem::size_of;

/*
    #[test]
    fn alloc() {
        let mut state = KyyMutator::new(100).unwrap();
        unsafe {
            state.alloc_slots(ORef::NULL, 5);
            state.alloc_bytes(ORef::NULL, 8, 8);
        }
    }

    #[test]
    fn collect() {
        let mut state = KyyMutator::new(10*size_of::<usize>()).unwrap();
        unsafe {
            state.alloc_slots(ORef::NULL, 5);
            state.alloc_slots(ORef::NULL, 5);
        }
    }
*/
}

