use std::convert::TryFrom;
use std::mem::transmute;
use std::ptr::{self, NonNull};

use super::object::Object;
use super::gc::Header;

#[derive(Debug, Eq)]
pub struct ObjectPtr<T>(NonNull<T>);

impl<T> Clone for ObjectPtr<T> {
    fn clone(&self) -> Self { *self }
}

impl<T> Copy for ObjectPtr<T> {}

impl<T> PartialEq for ObjectPtr<T> {
    fn eq(&self, other: &Self) -> bool { self.0 == other.0 }
}

impl<T> ObjectPtr<T> {
    pub unsafe fn from_raw(raw: *mut T) -> ObjectPtr<T> { ObjectPtr(NonNull::new_unchecked(raw)) }

    pub fn as_obj(self) -> ObjectPtr<Object> { unsafe { self.unchecked_cast() } }

    pub unsafe fn unchecked_cast<U>(self) -> ObjectPtr<U> { ObjectPtr(self.0.cast()) }

    /// Safety: The returned reference must not be live across a safepoint.
    pub unsafe fn as_ref(&self) -> &T { self.0.as_ref() }

    /// Safety: The returned pointer must not be live across a safepoint.
    pub unsafe fn as_ptr(&self) -> *const T { self.0.as_ptr() as _ }

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

    pub fn class(self) -> ObjectRef { unsafe { self.header().class() } }
}

// ---

// Can be null, unlike ObjectPtr<T>
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ObjectRef(*mut Object);

impl ObjectRef {
    pub const NULL: ObjectRef = ObjectRef(ptr::null_mut());

    fn is_null(self) -> bool { self.0 == ptr::null_mut() }

    pub unsafe fn unchecked_cast<T>(self) -> ObjectPtr<T> { ObjectPtr::from_raw(self.0 as *mut T) }

    pub fn is(self, other: ObjectRef) -> bool { ptr::eq(self.0, other.0) }
}

impl TryFrom<ObjectRef> for ObjectPtr<Object> {
    type Error = ();

    fn try_from(oref: ObjectRef) -> Result<ObjectPtr<Object>, ()> {
        if !oref.is_null() { unsafe { Ok(ObjectPtr::from_raw(oref.0)) } } else { Err(()) }
    }
}

impl<T> From<ObjectPtr<T>> for ObjectRef {
    fn from(mut obj: ObjectPtr<T>) -> ObjectRef { unsafe { ObjectRef(obj.0.as_mut() as *mut T as _) } }
}

