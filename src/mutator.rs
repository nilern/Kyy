use std::rc::Rc;
use std::cell::Cell;
use std::mem::{size_of, align_of, transmute};

use super::gc::{self, ORef, Gc, Header, Heap};
use super::int::Int;
use super::bool::Bool;
use super::tuple::Tuple;
use super::string::String;

pub struct Root<T>(Rc<Cell<Gc<T>>>);

impl<T> Clone for Root<T> {
    fn clone(&self) -> Root<T> { Root(self.0.clone()) }
}

impl<T> Root<T> {
    fn new(oref: Gc<T>) -> Root<T> { Root(Rc::new(Cell::new(oref))) }

    pub unsafe fn unchecked_cast<U>(self) -> Root<U> { transmute(self) }

    /// Safety: The returned reference must not be live across a safepoint.
    pub unsafe fn as_ref<'a>(&'a self) -> &'a T { transmute(self.0.get().as_ref()) }

    /// Safety: The returned pointer must not be live across a safepoint.
    pub unsafe fn as_mut_ptr(&self) -> *mut T { self.0.get().as_mut_ptr() }

    /// Safety: The returned oref must not be live across a safepoint.
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

    fn isa(km: &KyyMutator, root: Root<()>) -> bool {
        unsafe { root.oref().unchecked_cast::<()>().class().is(Self::reify(km).oref().into()) }
    }

    fn downcast(km: &KyyMutator, root: Root<()>) -> Option<Root<Self>> {
        if Self::isa(km, root.clone()) {
            Some(unsafe { root.unchecked_cast() })
        } else {
            None
        }
    }
}

pub unsafe trait KyySizedBytesType: KyyType {
    fn new(km: &mut KyyMutator, contents: Self) -> Root<Self> {
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
    bool_typ: Gc<Type>,
    tuple_typ: Gc<Type>,
    string_typ: Gc<Type>
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
impl_kyy_type!(Bool, bool_typ);
impl_kyy_type!(Tuple, tuple_typ);
impl_kyy_type!(String, string_typ);

// ---

pub struct KyyMutator {
    heap: gc::Heap,
    roots: Vec<Root<()>>,
    types: BuiltinTypes,
    singletons: Singletons
}

struct Singletons {
    tru: Root<Bool>,
    fals: Root<Bool>
}

impl KyyMutator {
    pub fn new(max_heap_size: usize) -> Option<KyyMutator> {
        let mut heap = gc::Heap::new(max_heap_size)?;
        let mut roots = Vec::new();
        unsafe {
            let mut type_typ: Gc<Type> = heap.alloc_bytes(ORef::NULL, size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();
            *type_typ.header_mut().class_mut() = type_typ.into();
            let int_typ: Gc<Type> = heap.alloc_bytes(type_typ.into(), size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();
            let bool_typ: Gc<Type> = heap.alloc_bytes(type_typ.into(), size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();
            let tuple_typ: Gc<Type> = heap.alloc_bytes(type_typ.into(), size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();
            let string_typ: Gc<Type> = heap.alloc_bytes(type_typ.into(), size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();

            let mut tru: Gc<Bool> = heap.alloc_bytes(bool_typ.into(), size_of::<Bool>(), align_of::<Bool>())?
                .unchecked_cast();
            tru.as_mut_ptr().write(Bool(true));
            let tru = Root::new(tru);
            roots.push(tru.clone().unchecked_cast());
            let mut fals: Gc<Bool> = heap.alloc_bytes(bool_typ.into(), size_of::<Bool>(), align_of::<Bool>())?
                .unchecked_cast();
            fals.as_mut_ptr().write(Bool(false));
            let fals = Root::new(fals);
            roots.push(fals.clone().unchecked_cast());

            Some(KyyMutator {
                heap,
                roots,
                types: BuiltinTypes {type_typ, int_typ, bool_typ, tuple_typ, string_typ},
                singletons: Singletons {tru, fals}
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

    pub fn the_true(&self) -> Root<Bool> { self.singletons.tru.clone() }
    pub fn the_false(&self) -> Root<Bool> { self.singletons.fals.clone() }

    unsafe fn gc(&mut self) {
        self.roots.retain(|root| Rc::strong_count(&root.0) > 1);

        for root in self.roots.iter() {
            root.mark(&mut self.heap);
        }

        let BuiltinTypes {ref mut type_typ, ref mut int_typ, ref mut bool_typ, ref mut tuple_typ, ref mut string_typ} =
            self.types;
        for typ in [type_typ, int_typ, bool_typ, tuple_typ, string_typ].iter_mut() {
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

