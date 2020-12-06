use super::mutator::Root;

#[repr(C)]
pub struct Bool(pub bool);

impl From<Root<Bool>> for bool {
    fn from(b: Root<Bool>) -> bool { unsafe { b.as_ref().0 } }
}

#[cfg(test)]
mod tests {
    use super::super::mutator::KyyMutator;

    #[test]
    fn singletons() {
        let km = KyyMutator::new(1000).unwrap();

        assert_eq!(bool::from(km.the_true()), true);
        assert_eq!(bool::from(km.the_false()), false);
    }
}

