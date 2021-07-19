use std::cell::Cell;
use std::mem::transmute;
use std::rc::Rc;

use super::gc::Heap;
use super::object::Object;
use super::orefs::Gc;

pub struct Handle<T>(Rc<Cell<Gc<T>>>);

impl<T> Clone for Handle<T> {
    fn clone(&self) -> Handle<T> { Handle(self.0.clone()) }
}

impl<T> Handle<T> {
    pub unsafe fn untracked(oref: Gc<T>) -> Handle<T> { Handle(Rc::new(Cell::new(oref))) }

    pub fn is_active(&self) -> bool { Rc::strong_count(&self.0) > 1 }

    pub fn as_obj(self) -> Handle<Object> { unsafe { self.unchecked_cast() } }

    pub unsafe fn unchecked_cast<U>(self) -> Handle<U> { transmute(self) }

    /// Safety: The returned reference must not be live across a safepoint.
    pub unsafe fn as_ref<'a>(&'a self) -> &'a T { transmute(self.0.get().as_ref()) }

    /// Safety: The returned pointer must not be live across a safepoint.
    pub unsafe fn as_mut_ptr(&self) -> *mut T { self.0.get().as_mut_ptr() }

    /// Safety: The returned oref must not be live across a safepoint.
    pub unsafe fn oref(&self) -> Gc<T> { self.0.get() }

    pub unsafe fn mark(&self, heap: &mut Heap) {
        self.0.set(heap.mark_root(self.0.get().into()).unchecked_cast())
    }
}

