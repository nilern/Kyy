use std::rc::Rc;
use std::cell::Cell;
use std::mem::{size_of, align_of, transmute};

use super::gc::{self, ORef, Gc, Header, Heap};
use super::int::Int;
use super::tuple::Tuple;

#[derive(Debug, Clone)]
pub struct Root<T>(Rc<Cell<Gc<T>>>);

impl<T> Root<T> {
    fn new(oref: Gc<T>) -> Root<T> { Root(Rc::new(Cell::new(oref))) }

    pub unsafe fn unchecked_cast<U>(self) -> Root<U> { transmute(self) }

    /// Safety: The returned reference must not be live across a safepoint.
    pub unsafe fn as_ref<'a>(&'a self) -> &'a T { transmute(self.0.get().as_ref()) }

    /// Safety: The returned pointer must not be live across a safepoint.
    pub unsafe fn as_mut_ptr(&self) -> *mut T { self.0.get().as_mut_ptr() }

    pub unsafe fn oref(&self) -> Gc<T> { self.0.get() }

    unsafe fn mark(&self, heap: &mut Heap) {
        self.0.set(heap.mark_root(self.0.get().into()).unchecked_cast())
    }
}

// ---

#[repr(C)]
pub struct Type {}

// ---

pub unsafe trait KyyType: Sized {
    fn reify(km: &KyyMutator) -> Root<Type>;

    /// Safety: The returned reference must not be live across a safepoint.
    unsafe fn header<'a>(&'a self) -> &'a Header {
        transmute(Gc::from_raw(transmute::<&Self, *mut Self>(self)).header())
    }
}

pub unsafe trait KyyBytesType: KyyType {
    fn new(km: &mut KyyMutator, contents: Self) -> Root<Self> where Self: Sized {
        unsafe {
            let root: Root<()> = km.alloc_bytes(Self::reify(km), size_of::<Self>(), align_of::<Self>());
            let root: Root<Self> = root.unchecked_cast();
            root.as_mut_ptr().write(contents);
            root
        }
    }
}

#[derive(Clone, Copy)]
struct BuiltinTypes {
    type_typ: Gc<Type>,
    int_typ: Gc<Type>,
    tuple_typ: Gc<Type>
}

macro_rules! impl_kyy_type {
    ($T: ty, $field: ident) => {
        unsafe impl KyyType for $T {
            fn reify(km: &KyyMutator) -> Root<Type> { Root::new(km.types.$field) }
        }
    }
}

impl_kyy_type!(Type, type_typ);
impl_kyy_type!(Int, int_typ);
impl_kyy_type!(Tuple, tuple_typ);

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
            let mut type_typ: Gc<Type> = heap.alloc_bytes(ORef::NULL, size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();
            type_typ.set_class(type_typ.into());
            let int_typ: Gc<Type> = heap.alloc_bytes(type_typ.into(), size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();
            let tuple_typ: Gc<Type> = heap.alloc_bytes(type_typ.into(), size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();
            Some(KyyMutator {
                heap,
                roots: Vec::new(),
                types: BuiltinTypes {type_typ, int_typ, tuple_typ}
            })
        }
    }

    pub unsafe fn alloc_slots(&mut self, class: Root<Type>, len: usize) -> Root<()> {
        let oref = self.heap.alloc_slots(class.oref().into(), len).unwrap_or_else(|| {
            self.gc();
            self.heap.alloc_slots(class.oref().into(), len).expect("Kyy out of memory")
        });
        let root = Root::new(oref);
        self.roots.push(root.clone());
        root
    }

    pub unsafe fn alloc_bytes(&mut self, class: Root<Type>, size: usize, align: usize) -> Root<()> {
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
        let BuiltinTypes {ref mut type_typ, ref mut int_typ, ref mut tuple_typ} = self.types;
        for typ in [type_typ, int_typ, tuple_typ].iter_mut() {
            **typ = typ.mark(&mut self.heap);
        }

        self.heap.gc();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::mem::size_of;

/*
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

