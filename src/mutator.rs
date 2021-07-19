use std::mem::{size_of, align_of, transmute};
use std::slice;

use super::granule::GSize;
use super::handle::Handle;
use super::orefs::{ObjectRef, ObjectPtr};
use super::gc::{self, Header};
use super::object::Object;
use super::int::Int;
use super::bool::Bool;
use super::tuple::Tuple;
use super::string::String;
use super::ast;

// FIXME: If the various `new` methods here and in e.g. super::ast do a GC,
// they do not consider the passed-in contents of the new object as roots

// ---

#[repr(C)]
pub struct Type {
    bases: ObjectPtr<Tuple>
}

// ---

pub unsafe trait KyyType: Sized {
    fn reify(km: &KyyMutator) -> Handle<Type>;

    /// Safety: The returned reference must not be live across a safepoint.
    unsafe fn header<'a>(&'a self) -> &'a Header {
        transmute(ObjectPtr::from_raw(transmute::<&Self, *mut Self>(self)).header())
    }

    fn isa(km: &KyyMutator, root: Handle<Object>) -> bool {
        unsafe { root.oref().as_obj().class().is(Self::reify(km).oref().into()) }
    }

    fn downcast(km: &KyyMutator, root: Handle<Object>) -> Option<Handle<Self>> {
        if Self::isa(km, root.clone()) {
            Some(unsafe { root.unchecked_cast() })
        } else {
            None
        }
    }
}

pub unsafe trait KyySizedSlotsType: KyyType {
    fn alloc(km: &mut KyyMutator, contents: Self) -> Handle<Self> {
        unsafe {
            let root: Handle<Object> = km.alloc_slots(Self::reify(km), GSize::of::<Self>().into());
            let root: Handle<Self> = root.unchecked_cast();
            root.as_mut_ptr().write(contents);
            root
        }
    }
}

pub unsafe trait KyySizedBytesType: KyyType {
    fn alloc(km: &mut KyyMutator, contents: Self) -> Handle<Self> {
        unsafe {
            let root: Handle<Object> = km.alloc_bytes(Self::reify(km), size_of::<Self>(), align_of::<Self>());
            let root: Handle<Self> = root.unchecked_cast();
            root.as_mut_ptr().write(contents);
            root
        }
    }
}

#[derive(Clone, Copy)]
struct BuiltinTypes {
    type_typ: ObjectPtr<Type>,
    object_typ: ObjectPtr<Type>,
    int_typ: ObjectPtr<Type>,
    bool_typ: ObjectPtr<Type>,
    tuple_typ: ObjectPtr<Type>,
    string_typ: ObjectPtr<Type>,

    expr_typ: ObjectPtr<Type>,
    add_typ: ObjectPtr<Type>,
    sub_typ: ObjectPtr<Type>,
    mul_typ: ObjectPtr<Type>,
    div_typ: ObjectPtr<Type>,
    le_typ: ObjectPtr<Type>,
    lt_typ: ObjectPtr<Type>,
    eq_typ: ObjectPtr<Type>,
    ne_typ: ObjectPtr<Type>,
    gt_typ: ObjectPtr<Type>,
    ge_typ: ObjectPtr<Type>,
    var_typ: ObjectPtr<Type>,
    const_typ: ObjectPtr<Type>,

    stmt_typ: ObjectPtr<Type>,
    expr_stmt_typ: ObjectPtr<Type>,
    if_typ: ObjectPtr<Type>,
    assign_typ: ObjectPtr<Type>
}

macro_rules! impl_kyy_type {
    ($T: ty, $field: ident) => {
        unsafe impl KyyType for $T {
            fn reify(km: &KyyMutator) -> Handle<Type> { unsafe { Handle::untracked(km.types.$field) } }
        }
    }
}

impl_kyy_type!(Type, type_typ);
impl_kyy_type!(Object, object_typ);
impl_kyy_type!(Int, int_typ);
impl_kyy_type!(Bool, bool_typ);
impl_kyy_type!(Tuple, tuple_typ);
impl_kyy_type!(String, string_typ);

impl_kyy_type!(ast::Expr, expr_typ);
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

impl_kyy_type!(ast::Stmt, stmt_typ);
impl_kyy_type!(ast::ExprStmt, expr_stmt_typ);
impl_kyy_type!(ast::If, if_typ);
impl_kyy_type!(ast::Assign, assign_typ);

// ---

pub struct KyyMutator {
    heap: gc::Heap,
    roots: Vec<Handle<Object>>,
    types: BuiltinTypes,
    singletons: Singletons
}

struct Singletons {
    tru: Handle<Bool>,
    fals: Handle<Bool>
}

impl KyyMutator {
    pub fn new(max_heap_size: usize) -> Option<KyyMutator> {
        // TODO: Smaller initial size when becomes self-expanding:
        let mut heap = gc::Heap::new(max_heap_size, max_heap_size)?;
        let mut roots = Vec::new();
        unsafe {
            let mut type_typ: ObjectPtr<Type> = heap.alloc_slots(ObjectRef::NULL, size_of::<Type>())?
                .unchecked_cast();
            let mut object_typ: ObjectPtr<Type> = heap.alloc_slots(type_typ.into(), size_of::<Type>())?
                .unchecked_cast();
            let mut tuple_typ: ObjectPtr<Type> = heap.alloc_slots(type_typ.into(), size_of::<Type>())?
                .unchecked_cast();
          
            // OPTIMIZE: Make the empty tuple a singleton?:
            let empty_bases: ObjectPtr<Tuple> = heap.alloc_slots(tuple_typ.into(), 0)?.unchecked_cast();
            let mut obj_bases: ObjectPtr<Tuple> = heap.alloc_slots(tuple_typ.into(), 1)?.unchecked_cast();
            {
                let obj_bases: &mut [ObjectPtr<Object>] = slice::from_raw_parts_mut(
                    obj_bases.as_mut_ptr() as *mut ObjectPtr<Object>, 1);
                obj_bases.copy_from_slice(&[object_typ.as_obj()]);
            }

            // Backpatch `<class 'type'>` header:
            *type_typ.header_mut().class_mut() = type_typ.into();

            // Backpatch type object contents:
            type_typ.as_mut_ptr().write(Type {bases: obj_bases});
            object_typ.as_mut_ptr().write(Type {bases: empty_bases});
            tuple_typ.as_mut_ptr().write(Type {bases: obj_bases});

            let mut int_typ: ObjectPtr<Type> = heap.alloc_slots(type_typ.into(), size_of::<Type>())?
                .unchecked_cast();
            int_typ.as_mut_ptr().write(Type {bases: obj_bases});
            let mut bool_typ: ObjectPtr<Type> = heap.alloc_slots(type_typ.into(), size_of::<Type>())?
                .unchecked_cast();
            bool_typ.as_mut_ptr().write(Type {bases: obj_bases});
            let mut string_typ: ObjectPtr<Type> = heap.alloc_slots(type_typ.into(), size_of::<Type>())?
                .unchecked_cast();
            string_typ.as_mut_ptr().write(Type {bases: obj_bases});

            let mut expr_typ: ObjectPtr<Type> = heap.alloc_slots(type_typ.into(), size_of::<Type>())?
                .unchecked_cast();
            expr_typ.as_mut_ptr().write(Type {bases: obj_bases});

            let mut expr_bases: ObjectPtr<Tuple> = heap.alloc_slots(tuple_typ.into(), 1)?.unchecked_cast();
            {
                let expr_bases: &mut [ObjectPtr<Object>] = slice::from_raw_parts_mut(
                    expr_bases.as_mut_ptr() as *mut ObjectPtr<Object>, 1);
                expr_bases.copy_from_slice(&[expr_typ.as_obj()]);
            }

            let mut add_typ: ObjectPtr<Type> = heap.alloc_slots(type_typ.into(), size_of::<Type>())?
                .unchecked_cast();
            add_typ.as_mut_ptr().write(Type {bases: expr_bases});
            let mut sub_typ: ObjectPtr<Type> = heap.alloc_slots(type_typ.into(), size_of::<Type>())?
                .unchecked_cast();
            sub_typ.as_mut_ptr().write(Type {bases: expr_bases});
            let mut mul_typ: ObjectPtr<Type> = heap.alloc_slots(type_typ.into(), size_of::<Type>())?
                .unchecked_cast();
            mul_typ.as_mut_ptr().write(Type {bases: expr_bases});
            let mut div_typ: ObjectPtr<Type> = heap.alloc_slots(type_typ.into(), size_of::<Type>())?
                .unchecked_cast();
            div_typ.as_mut_ptr().write(Type {bases: expr_bases});

            let mut le_typ: ObjectPtr<Type> = heap.alloc_slots(type_typ.into(), size_of::<Type>())?
                .unchecked_cast();
            le_typ.as_mut_ptr().write(Type {bases: expr_bases});
            let mut lt_typ: ObjectPtr<Type> = heap.alloc_slots(type_typ.into(), size_of::<Type>())?
                .unchecked_cast();
            lt_typ.as_mut_ptr().write(Type {bases: expr_bases});
            let mut eq_typ: ObjectPtr<Type> = heap.alloc_slots(type_typ.into(), size_of::<Type>())?
                .unchecked_cast();
            eq_typ.as_mut_ptr().write(Type {bases: expr_bases});
            let mut ne_typ: ObjectPtr<Type> = heap.alloc_slots(type_typ.into(), size_of::<Type>())?
                .unchecked_cast();
            ne_typ.as_mut_ptr().write(Type {bases: expr_bases});
            let mut ge_typ: ObjectPtr<Type> = heap.alloc_slots(type_typ.into(), size_of::<Type>())?
                .unchecked_cast();
            ge_typ.as_mut_ptr().write(Type {bases: expr_bases});
            let mut gt_typ: ObjectPtr<Type> = heap.alloc_slots(type_typ.into(), size_of::<Type>())?
                .unchecked_cast();
            gt_typ.as_mut_ptr().write(Type {bases: expr_bases});
            let mut var_typ: ObjectPtr<Type> = heap.alloc_slots(type_typ.into(), size_of::<Type>())?
                .unchecked_cast();
            var_typ.as_mut_ptr().write(Type {bases: expr_bases});
            let mut const_typ: ObjectPtr<Type> = heap.alloc_slots(type_typ.into(), size_of::<Type>())?
                .unchecked_cast();
            const_typ.as_mut_ptr().write(Type {bases: expr_bases});

            let mut stmt_typ: ObjectPtr<Type> = heap.alloc_slots(type_typ.into(), size_of::<Type>())?
                .unchecked_cast();
            stmt_typ.as_mut_ptr().write(Type {bases: obj_bases});

            let mut stmt_bases: ObjectPtr<Tuple> = heap.alloc_slots(tuple_typ.into(), 1)?.unchecked_cast();
            {
                let stmt_bases: &mut [ObjectPtr<Object>] = slice::from_raw_parts_mut(
                    stmt_bases.as_mut_ptr() as *mut ObjectPtr<Object>, 1);
                stmt_bases.copy_from_slice(&[stmt_typ.as_obj()]);
            }

            let mut expr_stmt_typ: ObjectPtr<Type> = heap.alloc_slots(type_typ.into(), size_of::<Type>())?
                .unchecked_cast();
            expr_stmt_typ.as_mut_ptr().write(Type {bases: stmt_bases});
            let mut if_typ: ObjectPtr<Type> = heap.alloc_slots(type_typ.into(), size_of::<Type>())?
                .unchecked_cast();
            if_typ.as_mut_ptr().write(Type {bases: stmt_bases});
            let mut assign_typ: ObjectPtr<Type> = heap.alloc_slots(type_typ.into(), size_of::<Type>())?
                .unchecked_cast();
            assign_typ.as_mut_ptr().write(Type {bases: stmt_bases});

            let mut tru: ObjectPtr<Bool> = heap.alloc_bytes(bool_typ.into(), size_of::<Bool>(), align_of::<Bool>())?
                .unchecked_cast();
            tru.as_mut_ptr().write(Bool(true));
            let tru = Handle::untracked(tru);
            roots.push(tru.clone().unchecked_cast());
            let mut fals: ObjectPtr<Bool> = heap.alloc_bytes(bool_typ.into(), size_of::<Bool>(), align_of::<Bool>())?
                .unchecked_cast();
            fals.as_mut_ptr().write(Bool(false));
            let fals = Handle::untracked(fals);
            roots.push(fals.clone().unchecked_cast());

            Some(KyyMutator {
                heap,
                roots,
                types: BuiltinTypes {type_typ, object_typ, int_typ, bool_typ, tuple_typ, string_typ,
                    expr_typ, add_typ, sub_typ, mul_typ, div_typ, le_typ, lt_typ, eq_typ, ne_typ, ge_typ, gt_typ,
                    var_typ, const_typ,
                    stmt_typ, expr_stmt_typ, if_typ, assign_typ},
                singletons: Singletons {tru, fals}
            })
        }
    }

    pub unsafe fn alloc_slots(&mut self, class: Handle<Type>, len: usize) -> Handle<Object> {
        let oref = self.heap.alloc_slots(class.oref().into(), len).unwrap_or_else(|| {
            self.gc();
            self.heap.alloc_slots(class.oref().into(), len).expect("Kyy out of memory")
        });
        self.root(oref)
    }

    pub unsafe fn alloc_bytes(&mut self, class: Handle<Type>, size: usize, align: usize) -> Handle<Object> {
        let oref = self.heap.alloc_bytes(class.oref().into(), size, align).unwrap_or_else(|| {
            self.gc();
            self.heap.alloc_bytes(class.oref().into(), size, align).expect("Kyy out of memory")
        });
        self.root(oref)
    }

    pub fn root<T>(&mut self, oref: ObjectPtr<T>) -> Handle<T> {
        let root = unsafe { Handle::untracked(oref) }; // Safety: untracked -> tracked on next line
        self.roots.push(root.clone().as_obj());
        root
    }

    pub fn the_true(&self) -> Handle<Bool> { self.singletons.tru.clone() }
    pub fn the_false(&self) -> Handle<Bool> { self.singletons.fals.clone() }

    unsafe fn gc(&mut self) {
        self.heap.prepare_gc();

        self.roots.retain(|root| root.is_active());

        for root in self.roots.iter() {
            root.mark(&mut self.heap);
        }

        let builtin_types: &mut [ObjectPtr<Type>] = slice::from_raw_parts_mut(
            transmute(&mut self.types), GSize::of::<BuiltinTypes>().into());
        for typ in builtin_types.iter_mut() {
            *typ = self.heap.mark_root((*typ).into()).unchecked_cast()
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
            state.alloc_slots(ObjectRef::NULL, 5);
            state.alloc_slots(ObjectRef::NULL, 5);
        }
    }
*/
}

