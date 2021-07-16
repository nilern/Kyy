use super::object::Object;
use super::orefs::{Gc, Root};
use super::mutator::{KyySizedSlotsType, KyyMutator};
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

        impl Root<$name> {
            pub fn left(self, km: &KyyMutator) -> Root<Object> { km.root(self.as_ref().left) }

            pub fn right(self, km: &KyyMutator) -> Root<Object> { km.root(self.as_ref().right) }
        }
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

impl Root<Var> {
    pub fn name(self, km: &KyyMutator) -> Root<String> { km.root(self.as_ref().0) }
}

#[repr(C)]
pub struct Const(pub Gc<Object>);

unsafe impl KyySizedSlotsType for Const {}

impl Root<Const> {
    pub fn value(self, km: &KyyMutator) -> Root<Object> { km.root(self.as_ref().0) }
}

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

