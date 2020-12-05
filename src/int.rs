use super::mutator::{KyyBytesType, KyyMutator, Root};

#[repr(C)]
pub struct Int(isize);

unsafe impl KyyBytesType for Int {}

impl From<Root<Int>> for isize {
    fn from(hdl: Root<Int>) -> isize { unsafe { hdl.as_ref().0 } }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn alloc() {
        let mut km = KyyMutator::new(100).unwrap();
        let n = 12345;
        let m = unsafe { Int::new(&mut km, Int(n)) };
        assert_eq!(isize::from(m), n);
    }
}

