use super::orefs::{Gc, Handle};
use super::mutator::{KyySizedBytesType, KyyMutator};

#[repr(C)]
pub struct Int(pub isize);

unsafe impl KyySizedBytesType for Int {}

impl Int {
    pub fn new(km: &mut KyyMutator, n: isize) -> Handle<Int> { Self::alloc(km, Int(n)) }
}

impl From<Gc<Int>> for isize {
    fn from(oref: Gc<Int>) -> isize { unsafe { oref.as_ref().0 } }
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

