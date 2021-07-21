use super::handle::Handle;
use super::mutator::KyyMutator;
use super::orefs::ObjectPtr;

#[repr(C)]
pub struct Bool(pub bool);

impl Bool {
    pub fn new(km: &KyyMutator, b: bool) -> Handle<Bool> {
        if b { km.the_true() } else { km.the_false() }
    }

    pub fn from_primitive(b: bool, the_true: ObjectPtr<Bool>, the_false: ObjectPtr<Bool>)
        -> ObjectPtr<Bool>
    {
        if b { the_true } else { the_false }
    }
}

impl From<Handle<Bool>> for bool {
    fn from(b: Handle<Bool>) -> bool { unsafe { b.as_ref().0 } }
}

#[cfg(test)]
mod tests {
    use super::*;

    use super::super::mutator::KyyMutator;

    #[test]
    fn singletons() {
        let km = KyyMutator::new(1 << 22).unwrap();

        assert_eq!(bool::from(Bool::new(&km, true)), true);
        assert_eq!(bool::from(Bool::new(&km, false)), false);
    }
}

