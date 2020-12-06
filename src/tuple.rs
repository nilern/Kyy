use std::mem::transmute;
use std::slice;

use super::gc::{GSize, Header, Gc};
use super::mutator::{KyyType, KyyMutator, Root};
use super::object::Object;

#[repr(C)]
pub struct Tuple;

impl Tuple {
    pub fn new(km: &mut KyyMutator, vs: &[Gc<Object>]) -> Root<Tuple> {
        unsafe {
            let root: Root<Object> = km.alloc_slots(Self::reify(km), vs.len());
            let root: Root<Self> = root.unchecked_cast();
            let contents: &mut [Gc<Object>] = slice::from_raw_parts_mut(
                root.as_mut_ptr() as *mut Gc<Object>, vs.len());
            contents.copy_from_slice(vs);
            root
        }
    }

    pub fn len(&self) -> usize {
        (unsafe { self.header().gsize() } - GSize::of::<Header>()).into()
    }

    /// Safety: the returned slice must not be live across a safepoint
    pub unsafe fn slots(&self) -> &[Gc<Object>] {
        slice::from_raw_parts(transmute(self), self.len())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use super::super::gc::ORef;
    use super::super::mutator::{KyyMutator, KyySizedBytesType};
    use super::super::int::Int;

    #[test]
    fn alloc() {
        let mut km = KyyMutator::new(1000).unwrap();
        let vs = [Int::new(&mut km, Int(3)), Int::new(&mut km, Int(2)), Int::new(&mut km, Int(1))];
        let tvs: Vec<Gc<Object>> = vs.iter().map(|root| unsafe { root.oref().unchecked_cast() }).collect();
        let tup = Tuple::new(&mut km, &tvs);

        unsafe {
            assert_eq!(tup.as_ref().len(), 3);
            for (tv, v) in tup.as_ref().slots().iter().zip(vs.iter()) {
                assert!(ORef::from(*tv).is(v.oref().into()));
            }
        }
    }
}

