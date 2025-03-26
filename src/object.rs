use std::alloc::{alloc, dealloc, handle_alloc_error, Layout};
use std::collections::HashMap;

pub(crate) struct Object {
    next: *mut Object,
    object_type: ObjectType
}

impl Object {
    fn string(text: String) -> Self {
	Self {
	    next: std::ptr::null_mut(),
	    object_type: ObjectType::String(text)
	}
    }

    pub(crate) fn object_type(&self) -> &ObjectType {
	&self.object_type
    }
}

impl <'a> PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
	match (&self.object_type, &other.object_type) {
	    (ObjectType::String(s1), ObjectType::String(s2)) => s1 == s2
	}
    }
}

#[derive(Clone)]
pub(crate) enum ObjectType {
    String(String)
}

pub(crate) struct Heap {
    objects: *mut Object
}

impl Heap {
    pub(crate) fn new() -> Self {
	Self { objects: std::ptr::null_mut() }
    }
    
    pub(crate) fn allocate_string(&mut self, text: String) -> *mut Object {
	unsafe {
	    let layout = Layout::new::<Object>();
	    let ptr = alloc(layout);

	    if ptr.is_null() {
		handle_alloc_error(layout);
	    }

	    std::ptr::write(ptr as *mut Object, Object::string(text));
	    
	    (*(ptr as *mut Object)).next = self.objects;
	    self.objects = ptr as *mut Object;
		
	    ptr as *mut Object
	}
    }
    
    pub(crate) fn free(&mut self) {
	unsafe {
	    while !self.objects.is_null() {
		let next = (*self.objects).next;
		let layout = Layout::new::<Object>();

		std::ptr::drop_in_place(self.objects);
		
		dealloc(self.objects.cast::<u8>(), layout);

		self.objects = next;
	    }
	}
    }
    
}

impl Drop for Heap {
    fn drop(&mut self) {
	
	unsafe {
	    while !self.objects.is_null() {
		let next = (*self.objects).next;
		let layout = Layout::new::<Object>();

		std::ptr::drop_in_place(self.objects);
		
		dealloc(self.objects.cast::<u8>(), layout);

		self.objects = next;
	    }
	}
	

	// std::mem::drop(self.strings);
    }
}
