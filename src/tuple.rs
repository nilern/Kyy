use std::mem::transmute;
use std::slice;

use super::handle::Handle;
use super::orefs::Gc;
use super::mutator::{KyyType, KyyMutator};
use super::object::Object;

#[repr(C)]
pub struct Tuple;

impl Tuple {
    pub fn new(km: &mut KyyMutator, vs: &[Gc<Object>]) -> Handle<Tuple> {
        unsafe {
            let root: Handle<Object> = km.alloc_slots(Self::reify(km), vs.len());
            let root: Handle<Self> = root.unchecked_cast();
            let contents: &mut [Gc<Object>] = slice::from_raw_parts_mut(
                root.as_mut_ptr() as *mut Gc<Object>, vs.len());
            contents.copy_from_slice(vs);
            root
        }
    }
}

impl Handle<Tuple> {
    pub fn len(&self) -> usize { unsafe { self.as_ref().header().raw_size() } }

    /// Safety: the returned slice must not be live across a safepoint
    pub unsafe fn slots(&self) -> &[Gc<Object>] {
        slice::from_raw_parts(transmute(self), self.len())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use super::super::orefs::ORef;
    use super::super::mutator::KyyMutator;
    use super::super::int::Int;

    #[test]
    fn alloc() {
        let mut km = KyyMutator::new(1 << 22).unwrap();
        let vs = [Int::new(&mut km, 3), Int::new(&mut km, 2), Int::new(&mut km, 1)];
        let tvs: Vec<Gc<Object>> = vs.iter().map(|root| unsafe { root.oref().as_obj() }).collect();
        let tup = Tuple::new(&mut km, &tvs);

        unsafe {
            assert_eq!(tup.len(), 3);
            for (tv, v) in tup.slots().iter().zip(vs.iter()) {
                assert!(ORef::from(*tv).is(v.oref().into()));
            }
        }
    }
}

