use std::mem::size_of;
use std::ops::{Add, AddAssign, Sub};

pub struct Granule(usize);

// ---

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct GSize(usize);

impl From<usize> for GSize {
    fn from(n: usize) -> GSize { GSize(n) }
}

impl From<GSize> for usize {
    fn from(n: GSize) -> usize { n.0 }
}

impl Add for GSize {
    type Output = Self;

    fn add(self, other: Self) -> GSize { GSize(self.0 + other.0) }
}

impl AddAssign for GSize {
    fn add_assign(&mut self, other: Self) { *self = *self + other }
}

impl Sub for GSize {
    type Output = Self;

    fn sub(self, other: Self) -> GSize { GSize(self.0 - other.0) }
}

impl GSize {
    pub fn in_granules(size: usize) -> Option<GSize> {
        Some(GSize(size.checked_add(size_of::<Granule>() - 1)? / size_of::<Granule>()))
    }

    pub fn of<T>() -> GSize {
        GSize((size_of::<T>() + size_of::<Granule>() - 1) / size_of::<Granule>())
    }

    pub fn in_bytes(self) -> usize { self.0 * size_of::<Granule>() }
}

