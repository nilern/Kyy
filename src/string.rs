use std::fmt::{self, Debug, Formatter};
use std::mem::{size_of, align_of, transmute};
use std::slice;
use std::str;

use super::gc::Header;
use super::orefs::Root;
use super::mutator::{KyyMutator, KyyType};
use super::object::Object;

#[repr(C)]
pub struct String;

impl String {
    pub fn new(km: &mut KyyMutator, cs: &str) -> Root<String> {
        unsafe {
            let root: Root<Object> = km.alloc_bytes(Self::reify(km), cs.len(), align_of::<u8>());
            let root: Root<Self> = root.unchecked_cast();
            let contents: &mut [u8] = slice::from_raw_parts_mut(
                root.as_mut_ptr() as *mut u8, cs.len());
            contents.copy_from_slice(cs.as_bytes());
            root
        }
    }
}

impl Debug for Root<String> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result { self.as_str().fmt(f) }
}

impl Root<String> {
    pub fn len(self) -> usize {
        (unsafe { self.as_ref().header().size() } - size_of::<Header>()).into()
    }

    /// Safety: the returned slice must not be live across a safepoint
    pub unsafe fn as_str(&self) -> &str {
        str::from_utf8_unchecked(slice::from_raw_parts(transmute(self.as_ref()), self.clone().len()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn alloc() {
        let mut km = KyyMutator::new(1000).unwrap();
        let cs = "foo";
        let s = String::new(&mut km, &cs);

        unsafe { assert_eq!(s.as_ref().as_str(), cs); }
    }
}

