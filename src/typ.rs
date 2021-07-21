use std::convert::TryInto;

use super::gc::Heap;
use super::granule::GSize;
use super::orefs::ObjectPtr;
use super::tuple::Tuple;
use super::int::Int;
use super::bool::Bool;

#[repr(C)]
pub struct Type {
    is_bytes: ObjectPtr<Bool>,
    is_indexed: ObjectPtr<Bool>,
    size: ObjectPtr<Int>,
    align: ObjectPtr<Int>,
    bases: ObjectPtr<Tuple>
}

impl Type {
    unsafe fn new_oref(heap: &mut Heap,
        class: ObjectPtr<Type>, int_class: ObjectPtr<Type>,
        the_true: ObjectPtr<Bool>, the_false: ObjectPtr<Bool>,
        is_bytes: bool, is_indexed: bool, bases: ObjectPtr<Tuple>,
        size: usize, align: usize
    ) -> Option<ObjectPtr<Self>> {
        let is_bytes = Bool::from_primitive(is_bytes, the_true, the_false);
        let is_indexed = Bool::from_primitive(is_indexed, the_true, the_false);
        let size = Int::new_oref(heap, int_class, size.try_into().unwrap())?;
        let align = Int::new_oref(heap, int_class, align.try_into().unwrap())?;

        let typ: ObjectPtr<Self> = heap.alloc_slots(class.into(), GSize::of::<Self>().into())?
            .unchecked_cast();
        typ.as_mut_ptr().write(Self {is_bytes, is_indexed, size, align, bases});
        Some(typ)
    }
}

