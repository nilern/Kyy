use std::rc::Rc;
use std::cell::Cell;
use std::mem::{size_of, align_of, transmute};

use super::gc::{self, ORef, Gc, Heap};
use super::int::Int;

#[derive(Debug, Clone)]
pub struct Root<T>(Rc<Cell<Gc<T>>>);

impl<T> Root<T> {
    fn new(oref: Gc<T>) -> Root<T> { Root(Rc::new(Cell::new(oref))) }

    pub unsafe fn unchecked_downcast<U>(self) -> Root<U> { transmute(self) }

    /// Safety: The returned reference must not be live across a safepoint.
    pub unsafe fn as_ref<'a>(&'a self) -> &'a T { transmute(self.0.get().as_ref()) }

    /// Safety: The returned pointer must not be live across a safepoint.
    pub unsafe fn as_mut_ptr(&self) -> *mut T { self.0.get().as_mut_ptr() }

    unsafe fn oref(&self) -> Gc<T> { self.0.get() }

    unsafe fn mark(&self, heap: &mut Heap) {
        self.0.set(heap.mark_root(self.0.get().into()).unchecked_downcast())
    }
}

// ---

#[derive(Clone, Copy)]
#[repr(C)]
pub struct Class {}

// ---

pub unsafe trait KyyType: Copy {
    fn reify(km: &KyyMutator) -> Root<Class>;
}

pub unsafe trait KyyBytesType: KyyType {
    unsafe fn new(km: &mut KyyMutator, contents: Self) -> Root<Self> where Self: Sized {
        let root: Root<()> = km.alloc_bytes(Self::reify(km), size_of::<Self>(), align_of::<Self>());
        let root: Root<Self> = root.unchecked_downcast();
        root.as_mut_ptr().write(contents);
        root
    }
}

#[derive(Clone, Copy)]
struct BuiltinTypes {
    type_class: Gc<Class>,
    int_class: Gc<Class>
}

unsafe impl KyyType for Class {
    fn reify(km: &KyyMutator) -> Root<Class> {
        unsafe { Root::new(km.types.type_class) }
    }
}

unsafe impl KyyType for Int {
    fn reify(km: &KyyMutator) -> Root<Class> {
        unsafe { Root::new(km.types.int_class) }
    }
}

// ---

pub struct KyyMutator {
    heap: gc::Heap,
    roots: Vec<Root<()>>,
    types: BuiltinTypes
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
                roots: Vec::new(),
                types: BuiltinTypes {type_class, int_class}
            })
        }
    }

    pub unsafe fn alloc_slots(&mut self, class: Root<Class>, len: usize) -> Root<()> {
        let oref = self.heap.alloc_slots(class.oref().into(), len).unwrap_or_else(|| {
            self.gc();
            self.heap.alloc_slots(class.oref().into(), len).expect("Kyy out of memory")
        });
        let root = Root::new(oref);
        self.roots.push(root.clone());
        root
    }

    pub unsafe fn alloc_bytes(&mut self, class: Root<Class>, size: usize, align: usize) -> Root<()> {
        let oref = self.heap.alloc_bytes(class.oref().into(), size, align).unwrap_or_else(|| {
            self.gc();
            self.heap.alloc_bytes(class.oref().into(), size, align).expect("Kyy out of memory")
        });
        let root = Root::new(oref);
        self.roots.push(root.clone());
        root
    }

    unsafe fn gc(&mut self) {
        self.roots.retain(|root| Rc::strong_count(&root.0) > 1);
        for root in self.roots.iter() {
            root.mark(&mut self.heap);
        }
        let BuiltinTypes {ref mut type_class, ref mut int_class} = self.types;
        *type_class = type_class.mark(&mut self.heap);
        *int_class = int_class.mark(&mut self.heap);

        self.heap.gc();
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

