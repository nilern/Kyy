use super::orefs::ObjectPtr;
use super::tuple::Tuple;

#[repr(C)]
pub struct Type {
    pub bases: ObjectPtr<Tuple>
}

