use std::mem::{size_of, align_of};

use super::mutator::{KyyMutator, Root, KyyType};

#[repr(C)]
pub struct Int(isize);

impl Int {
    pub unsafe fn new(km: &mut KyyMutator, n: isize) -> Root<Int> {
        let handle: Root<()> = km.alloc_bytes(Int::reify(km), size_of::<Int>(), align_of::<Int>());
        let handle: Root<Int> = handle.unchecked_downcast();
        handle.as_mut_ptr().write(Int(n));
        handle
    }
}

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
        let m = unsafe { Int::new(&mut km, n) };
        assert_eq!(isize::from(m), n);
    }
}

