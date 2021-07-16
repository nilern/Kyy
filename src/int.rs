use super::orefs::{Gc, Root};
use super::mutator::{KyySizedBytesType, KyyMutator};

#[repr(C)]
pub struct Int(pub isize);

unsafe impl KyySizedBytesType for Int {}

impl Int {
    pub fn new(km: &mut KyyMutator, n: isize) -> Root<Int> { Self::alloc(km, Int(n)) }
}

impl From<Gc<Int>> for isize {
    fn from(oref: Gc<Int>) -> isize { unsafe { oref.as_ref().0 } }
}

impl From<Root<Int>> for isize {
    fn from(hdl: Root<Int>) -> isize { unsafe { hdl.as_ref().0 } }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn alloc() {
        let mut km = KyyMutator::new(1000).unwrap();
        let n = 12345;
        let m = Int::new(&mut km, Int(n));
        assert_eq!(isize::from(m), n);
    }
}

