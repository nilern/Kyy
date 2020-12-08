use std::sync::Arc;

use super::lexer::Spanning;
use super::object::Object;
use super::orefs::{Root, Gc};
use super::mutator::KyySizedSlotsType;
use super::string::String;
use super::tuple::Tuple;
use super::int::Int;

macro_rules! binary_node {
    ($name: ident) => {
        #[repr(C)]
        pub struct $name {
            filename: Gc<String>,
            offset: Gc<Int>,
            left: Gc<Object>,
            right: Gc<Object>
        }

        unsafe impl KyySizedSlotsType for $name {}
    }
}

binary_node!(Add);
binary_node!(Sub);
binary_node!(Mul);
binary_node!(Div); // FIXME: __truediv__ vs. __floordiv__

binary_node!(Le);
binary_node!(Lt);
binary_node!(Eq);
binary_node!(Ne);
binary_node!(Gt);
binary_node!(Ge);

#[repr(C)]
pub struct Var(pub Gc<String>);

unsafe impl KyySizedSlotsType for Var {}

#[repr(C)]
pub struct Const(pub Gc<Object>);

unsafe impl KyySizedSlotsType for Const {}

#[repr(C)]
pub struct If {
    condition: Gc<Object>,
    conseq: Gc<Tuple>,
    alt: Gc<Tuple>
}

unsafe impl KyySizedSlotsType for If {}

#[repr(C)]
pub struct Assign {
    left: Gc<String>,
    right: Gc<Object>,
}

unsafe impl KyySizedSlotsType for Assign {}

