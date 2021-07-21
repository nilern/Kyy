pub struct ObjectPtr<T>(*mut T);

impl<T> ObjectPtr<T> {
    pub unsafe fn from_raw(ptr: *mut T) -> Self { Self(ptr) }

    pub unsafe fn as_ptr(&self) -> *const T { self.0 as _ }
}

