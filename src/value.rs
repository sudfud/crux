use std::fmt;
use std::ops;

use crate::object::{Object, ObjectType};

#[derive(Clone, Copy)]
pub(crate) enum Value<'a> {
    Boolean(bool),
    Number(f64),
    Null,
    Object(*mut Object<'a>)
}

impl <'a> Value <'a> {
    pub(crate) fn is_falsey(&self) -> bool {
	match self {
	    Self::Boolean(b) => !b,
	    Self::Number(_) | Self::Object(_) => false,
	    Self::Null => true
	}
    }

    pub(crate) fn as_number(&self) -> Option<f64> {
	match self {
	    Self::Number(n) => Some(*n),
	    _ => None
	}
    }

    pub(crate) fn as_string(&self) -> Option<String> {
	match self {
	    Self::Object(o) => unsafe {
		match (**o).object_type() {
		    ObjectType::String(s) => Some(String::from(*s)),
		    _ => None
		}
	    },
	    _ => None
	}
    }
}

impl <'a> fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
	write!(
	    f,
	    "{}",
	    match self {
		Self::Boolean(b) => b.to_string(),
		Self::Number(n) => n.to_string(),
		Self::Null => String::from("null"),
		Self::Object(o) => unsafe {
		    match (**o).object_type() {
			ObjectType::String(s) => String::from(*s)
		    }
		}
	    }
	)
    }
}

impl <'a> PartialEq for Value<'a> {
    fn eq(&self, other: &Self) -> bool {
	match (self, other) {
	    (Self::Number(n1), Self::Number(n2)) => n1 == n2,
	    (Self::Boolean(b1), Self::Boolean(b2)) => b1 == b2,
	    (Self::Null, Self::Null) => true,
	    (Self::Object(o1), Self::Object(o2)) => unsafe {
		**o1 == **o2
	    },
	    _ => false
	}
    }
}

impl <'a> ops::Add for Value<'a> {
    type Output = Result<Self, &'static str>;

    fn add(self, rhs: Self) -> Self::Output {
	match (self, rhs) {
	    (Self::Number(a), Self::Number(b)) => return Ok(Self::Number(a + b)),
	    _ => Err("Operands must be numbers.")
	}
    }
}

impl <'a> ops::Sub for Value<'a> {
    type Output = Result<Self, &'static str>;

    fn sub(self, rhs: Self) -> Self::Output {
	match (self, rhs) {
	    (Self::Number(a), Self::Number(b)) => Ok(Self::Number(a - b)),
	    _ => Err("Operands must be numbers.")
	}
    }
}

impl <'a> ops::Mul for Value<'a> {
    type Output = Result<Self, &'static str>;

    fn mul(self, rhs: Self) -> Self::Output {
	match (self, rhs) {
	    (Self::Number(a), Self::Number(b)) => Ok(Self::Number(a * b)),
	    _ => Err("Operands must be numbers.")
	}
    }
}

impl <'a> ops::Div for Value<'a> {
    type Output = Result<Self, &'static str>;

    fn div(self, rhs: Self) -> Self::Output {
	match (self, rhs) {
	    (Self::Number(a), Self::Number(b)) => Ok(Self::Number(a / b)),
	    _ => Err("Operands must be numbers.")
	}
    }
}
