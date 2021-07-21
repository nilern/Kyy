use std::mem::{size_of, align_of};

use super::gc::Heap;
use super::handle::Handle;
use super::orefs::ObjectPtr;
use super::mutator::{KyySizedBytesType, KyyMutator};
use super::typ::Type;

#[repr(C)]
pub struct Int(pub isize);

unsafe impl KyySizedBytesType for Int {}

impl Int {
    pub fn new(km: &mut KyyMutator, n: isize) -> Handle<Int> { Self::alloc(km, Int(n)) }

    pub unsafe fn new_oref(heap: &mut Heap, class: ObjectPtr<Type>, n: isize)
        -> Option<ObjectPtr<Self>>
    {
        let res: ObjectPtr<Self> = heap.alloc_bytes(class.into(), size_of::<Self>(), align_of::<Self>())?
            .unchecked_cast();
        res.as_mut_ptr().write(Self(n));
        Some(res)
    }
}

impl From<ObjectPtr<Int>> for isize {
    fn from(oref: ObjectPtr<Int>) -> isize { unsafe { oref.as_ref().0 } }
}

impl From<Handle<Int>> for isize {
    fn from(hdl: Handle<Int>) -> isize { unsafe { hdl.as_ref().0 } }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn alloc() {
        let mut km = KyyMutator::new(1 << 22).unwrap();
        let n = 12345;
        let m = Int::new(&mut km, n);
        assert_eq!(isize::from(m), n);
    }
}

