use super::orefs::Root;
use super::mutator::KyyMutator;

#[repr(C)]
pub struct Bool(pub bool);

impl Bool {
    pub fn new(km: &KyyMutator, b: bool) -> Root<Bool> {
        if b { km.the_true() } else { km.the_false() }
    }
}

impl From<Root<Bool>> for bool {
    fn from(b: Root<Bool>) -> bool { unsafe { b.as_ref().0 } }
}

#[cfg(test)]
mod tests {
    use super::*;

    use super::super::mutator::KyyMutator;

    #[test]
    fn singletons() {
        let km = KyyMutator::new(1000).unwrap();

        assert_eq!(bool::from(Bool::new(&km, true)), true);
        assert_eq!(bool::from(Bool::new(&km, false)), false);
    }
}

