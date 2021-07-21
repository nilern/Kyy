use super::object::Object;

#[repr(C)]
pub struct Type {
    base: Object,
    size: usize,
    align: usize,
    is_indexed: bool
}

