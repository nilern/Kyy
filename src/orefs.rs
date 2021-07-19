use std::convert::TryFrom;
use std::mem::transmute;
use std::ptr::{self, NonNull};

use super::object::Object;
use super::gc::Header;

#[derive(Debug, Eq)]
pub struct Gc<T>(NonNull<T>);

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self { *self }
}

impl<T> Copy for Gc<T> {}

impl<T> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool { self.0 == other.0 }
}

impl<T> Gc<T> {
    pub unsafe fn from_raw(raw: *mut T) -> Gc<T> { Gc(NonNull::new_unchecked(raw)) }

    pub fn as_obj(self) -> Gc<Object> { unsafe { self.unchecked_cast() } }

    pub unsafe fn unchecked_cast<U>(self) -> Gc<U> { Gc(self.0.cast()) }

    /// Safety: The returned reference must not be live across a safepoint.
    pub unsafe fn as_ref(&self) -> &T { self.0.as_ref() }

    /// Safety: The returned pointer must not be live across a safepoint.
    pub unsafe fn as_mut_ptr(&mut self) -> *mut T { self.0.as_mut() as _ }

    /// Safety: The returned reference must not be live across a safepoint.
    pub unsafe fn header<'a>(&'a self) -> &'a Header {
        let ptr = self.0.as_ref() as *const T;
        transmute((ptr as *const Header).offset(-1))
    }

    /// Safety: The returned reference must not be live across a safepoint.
    pub unsafe fn header_mut<'a>(&'a mut self) -> &'a mut Header {
        let ptr = self.0.as_mut() as *mut T;
        transmute((ptr as *mut Header).offset(-1))
    }

    pub fn class(self) -> ORef { unsafe { self.header().class() } }

    pub fn is_marked(self) -> bool { unsafe { self.header().is_marked() } }
}

// ---

// Can be null, unlike Gc<T>
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ORef(*mut Object);

impl ORef {
    pub const NULL: ORef = ORef(ptr::null_mut());

    fn is_null(self) -> bool { self.0 == ptr::null_mut() }

    pub unsafe fn unchecked_cast<T>(self) -> Gc<T> { Gc::from_raw(self.0 as *mut T) }

    pub fn is(self, other: ORef) -> bool { ptr::eq(self.0, other.0) }
}

impl TryFrom<ORef> for Gc<Object> {
    type Error = ();

    fn try_from(oref: ORef) -> Result<Gc<Object>, ()> {
        if !oref.is_null() { unsafe { Ok(Gc::from_raw(oref.0)) } } else { Err(()) }
    }
}

impl<T> From<Gc<T>> for ORef {
    fn from(mut obj: Gc<T>) -> ORef { unsafe { ORef(obj.0.as_mut() as *mut T as _) } }
}

