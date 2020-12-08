use std::mem::{size_of, align_of, transmute};
use std::slice;

use super::granule::GSize;
use super::orefs::{ORef, Gc, Root};
use super::gc::{self, Header};
use super::object::Object;
use super::int::Int;
use super::bool::Bool;
use super::tuple::Tuple;
use super::string::String;
use super::ast;

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

    fn isa(km: &KyyMutator, root: Root<Object>) -> bool {
        unsafe { root.oref().as_obj().class().is(Self::reify(km).oref().into()) }
    }

    fn downcast(km: &KyyMutator, root: Root<Object>) -> Option<Root<Self>> {
        if Self::isa(km, root.clone()) {
            Some(unsafe { root.unchecked_cast() })
        } else {
            None
        }
    }
}

pub unsafe trait KyySizedSlotsType: KyyType {
    fn new(km: &mut KyyMutator, contents: Self) -> Root<Self> {
        unsafe {
            let root: Root<Object> = km.alloc_slots(Self::reify(km), GSize::of::<Self>().into());
            let root: Root<Self> = root.unchecked_cast();
            root.as_mut_ptr().write(contents);
            root
        }
    }
}

pub unsafe trait KyySizedBytesType: KyyType {
    fn new(km: &mut KyyMutator, contents: Self) -> Root<Self> {
        unsafe {
            let root: Root<Object> = km.alloc_bytes(Self::reify(km), size_of::<Self>(), align_of::<Self>());
            let root: Root<Self> = root.unchecked_cast();
            root.as_mut_ptr().write(contents);
            root
        }
    }
}

#[derive(Clone, Copy)]
struct BuiltinTypes {
    type_typ: Gc<Type>,
    object_typ: Gc<Type>,
    int_typ: Gc<Type>,
    bool_typ: Gc<Type>,
    tuple_typ: Gc<Type>,
    string_typ: Gc<Type>,

    add_typ: Gc<Type>,
    sub_typ: Gc<Type>,
    mul_typ: Gc<Type>,
    div_typ: Gc<Type>,
    le_typ: Gc<Type>,
    lt_typ: Gc<Type>,
    eq_typ: Gc<Type>,
    ne_typ: Gc<Type>,
    gt_typ: Gc<Type>,
    ge_typ: Gc<Type>,
    var_typ: Gc<Type>,
    const_typ: Gc<Type>,
    if_typ: Gc<Type>,
    assign_typ: Gc<Type>
}

macro_rules! impl_kyy_type {
    ($T: ty, $field: ident) => {
        unsafe impl KyyType for $T {
            fn reify(km: &KyyMutator) -> Root<Type> { unsafe { Root::untracked(km.types.$field) } }
        }
    }
}

impl_kyy_type!(Type, type_typ);
impl_kyy_type!(Object, object_typ);
impl_kyy_type!(Int, int_typ);
impl_kyy_type!(Bool, bool_typ);
impl_kyy_type!(Tuple, tuple_typ);
impl_kyy_type!(String, string_typ);

impl_kyy_type!(ast::Add, add_typ);
impl_kyy_type!(ast::Sub, sub_typ);
impl_kyy_type!(ast::Mul, mul_typ);
impl_kyy_type!(ast::Div, div_typ);
impl_kyy_type!(ast::Le, le_typ);
impl_kyy_type!(ast::Lt, lt_typ);
impl_kyy_type!(ast::Eq, eq_typ);
impl_kyy_type!(ast::Ne, ne_typ);
impl_kyy_type!(ast::Ge, ge_typ);
impl_kyy_type!(ast::Gt, gt_typ);
impl_kyy_type!(ast::Var, var_typ);
impl_kyy_type!(ast::Const, const_typ);
impl_kyy_type!(ast::If, if_typ);
impl_kyy_type!(ast::Assign, assign_typ);

// ---

pub struct KyyMutator {
    heap: gc::Heap,
    roots: Vec<Root<Object>>,
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
            let object_typ: Gc<Type> = heap.alloc_bytes(type_typ.into(), size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();
            let int_typ: Gc<Type> = heap.alloc_bytes(type_typ.into(), size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();
            let bool_typ: Gc<Type> = heap.alloc_bytes(type_typ.into(), size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();
            let tuple_typ: Gc<Type> = heap.alloc_bytes(type_typ.into(), size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();
            let string_typ: Gc<Type> = heap.alloc_bytes(type_typ.into(), size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();

            let add_typ: Gc<Type> = heap.alloc_bytes(type_typ.into(), size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();
            let sub_typ: Gc<Type> = heap.alloc_bytes(type_typ.into(), size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();
            let mul_typ: Gc<Type> = heap.alloc_bytes(type_typ.into(), size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();
            let div_typ: Gc<Type> = heap.alloc_bytes(type_typ.into(), size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();

            let le_typ: Gc<Type> = heap.alloc_bytes(type_typ.into(), size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();
            let lt_typ: Gc<Type> = heap.alloc_bytes(type_typ.into(), size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();
            let eq_typ: Gc<Type> = heap.alloc_bytes(type_typ.into(), size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();
            let ne_typ: Gc<Type> = heap.alloc_bytes(type_typ.into(), size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();
            let ge_typ: Gc<Type> = heap.alloc_bytes(type_typ.into(), size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();
            let gt_typ: Gc<Type> = heap.alloc_bytes(type_typ.into(), size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();
            let var_typ: Gc<Type> = heap.alloc_bytes(type_typ.into(), size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();
            let const_typ: Gc<Type> = heap.alloc_bytes(type_typ.into(), size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();
            let if_typ: Gc<Type> = heap.alloc_bytes(type_typ.into(), size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();
            let assign_typ: Gc<Type> = heap.alloc_bytes(type_typ.into(), size_of::<Type>(), align_of::<Type>())?
                .unchecked_cast();

            let mut tru: Gc<Bool> = heap.alloc_bytes(bool_typ.into(), size_of::<Bool>(), align_of::<Bool>())?
                .unchecked_cast();
            tru.as_mut_ptr().write(Bool(true));
            let tru = Root::untracked(tru);
            roots.push(tru.clone().unchecked_cast());
            let mut fals: Gc<Bool> = heap.alloc_bytes(bool_typ.into(), size_of::<Bool>(), align_of::<Bool>())?
                .unchecked_cast();
            fals.as_mut_ptr().write(Bool(false));
            let fals = Root::untracked(fals);
            roots.push(fals.clone().unchecked_cast());

            Some(KyyMutator {
                heap,
                roots,
                types: BuiltinTypes {type_typ, object_typ, int_typ, bool_typ, tuple_typ, string_typ,
                    add_typ, sub_typ, mul_typ, div_typ, le_typ, lt_typ, eq_typ, ne_typ, ge_typ, gt_typ,
                    var_typ, const_typ, if_typ, assign_typ},
                singletons: Singletons {tru, fals}
            })
        }
    }

    pub unsafe fn alloc_slots(&mut self, class: Root<Type>, len: usize) -> Root<Object> {
        let oref = self.heap.alloc_slots(class.oref().into(), len).unwrap_or_else(|| {
            self.gc();
            self.heap.alloc_slots(class.oref().into(), len).expect("Kyy out of memory")
        });
        self.root(oref)
    }

    pub unsafe fn alloc_bytes(&mut self, class: Root<Type>, size: usize, align: usize) -> Root<Object> {
        let oref = self.heap.alloc_bytes(class.oref().into(), size, align).unwrap_or_else(|| {
            self.gc();
            self.heap.alloc_bytes(class.oref().into(), size, align).expect("Kyy out of memory")
        });
        self.root(oref)
    }

    pub fn root<T>(&mut self, oref: Gc<T>) -> Root<T> {
        let root = Root::untracked(oref);
        self.roots.push(root.clone().as_obj());
        root
    }

    pub fn the_true(&self) -> Root<Bool> { self.singletons.tru.clone() }
    pub fn the_false(&self) -> Root<Bool> { self.singletons.fals.clone() }

    unsafe fn gc(&mut self) {
        self.roots.retain(|root| root.is_active());

        for root in self.roots.iter() {
            root.mark(&mut self.heap);
        }

        let builtin_types: &mut [Gc<Type>] = slice::from_raw_parts_mut(
            transmute(&mut self.types), GSize::of::<BuiltinTypes>().into());
        for typ in builtin_types.iter_mut() {
            *typ = typ.mark(&mut self.heap);
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

