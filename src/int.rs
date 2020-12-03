use std::mem::{size_of, align_of};

use super::state::{KyyState, Handle, KyyType};

#[repr(C)]
pub struct Int(isize);

impl Int {
    pub unsafe fn new(st: &mut KyyState, n: isize) -> Handle<Int> {
        let handle: Handle<()> = st.alloc_bytes(Int::reify(st), size_of::<Int>(), align_of::<Int>());
        let handle: Handle<Int> = handle.unchecked_downcast();
        handle.as_mut_ptr().write(Int(n));
        handle
    }
}

impl From<Handle<Int>> for isize {
    fn from(hdl: Handle<Int>) -> isize { unsafe { hdl.as_ref().0 } }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn alloc() {
        let mut st = KyyState::new(100).unwrap();
        let n = 12345;
        let m = unsafe { Int::new(&mut st, n) };
        assert_eq!(isize::from(m), n);
    }
}

