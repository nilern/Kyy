use super::orefs::ObjectPtr;
use super::typ::Type;

struct Header(usize);

impl Header {
    const MARK_MASK: usize = 0b1;
    const MARK_BIT: usize = 1;

    fn new(class: ObjectPtr<Type>) -> Self { Self(unsafe { class.as_ptr()  as usize }) }

    unsafe fn class_unchecked(&self) -> ObjectPtr<Type> {
        ObjectPtr::from_raw(self.0 as *mut Type)
    }

    fn forwarding<T>(dest: ObjectPtr<T>) -> Self {
        Self(unsafe { dest.as_ptr() as usize } | Self::MARK_BIT)
    }

    fn is_marked(&self) -> bool { self.0 & Self::MARK_BIT == Self::MARK_BIT }

    fn forwarded(&self) -> Option<ObjectPtr<Object>> {
        if self.is_marked() {
            Some(unsafe { ObjectPtr::from_raw((self.0 & !Self::MARK_MASK) as *mut Object) })
        } else {
            None
        }
    }
}

// ---

#[repr(C)]
pub struct Object {
    header: Header
}

impl Object {
    fn class(&self) -> ObjectPtr<Type> { unsafe { self.header.class_unchecked() } }
}

// ---

#[repr(C)]
pub struct IndexedObject {
    base: Object,
    len: usize
}

