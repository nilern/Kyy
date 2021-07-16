use super::orefs::Root;
use super::mutator::KyySizedBytesType;

#[repr(C)]
pub struct Int(pub isize);

unsafe impl KyySizedBytesType for Int {}

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

