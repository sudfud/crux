use std::alloc::{alloc, dealloc, handle_alloc_error, Layout};
use std::collections::HashSet;

pub(crate) struct Object<'a> {
    next: *mut Object<'a>,
    object_type: ObjectType<'a>
}

impl <'a> Object<'a> {
    fn string(text: &'a str) -> Self {
	Self {
	    next: std::ptr::null_mut(),
	    object_type: ObjectType::String(text)
	}
    }

    pub(crate) fn object_type(&self) -> &ObjectType {
	&self.object_type
    }
}

impl <'a> PartialEq for Object<'a> {
    fn eq(&self, other: &Self) -> bool {
	match (&self.object_type, &other.object_type) {
	    (ObjectType::String(s1), ObjectType::String(s2)) => s1 == s2
	}
    }
}

#[derive(Clone)]
pub(crate) enum ObjectType<'a> {
    String(&'a str)
}

pub(crate) struct Heap<'a> {
    objects: *mut Object<'a>,
    strings: HashSet<String>
}

impl <'a> Heap<'a> {
    pub(crate) fn new() -> Self {
	Self { objects: std::ptr::null_mut(), strings: HashSet::new() }
    }
    
    pub(crate) fn allocate_string(&mut self, text: &str) -> *mut Object<'a> {
	unsafe {
	    let layout = Layout::new::<Object>();
	    let ptr = alloc(layout);

	    if ptr.is_null() {
		handle_alloc_error(layout);
	    }

	    // let ptr = ptr.cast::<Object>();

	    if !self.strings.contains(text) {
		self.strings.insert(String::from(text));
	    }

	    std::ptr::write(ptr as *mut Object, Object::string(self.strings.get(text).unwrap()));

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
