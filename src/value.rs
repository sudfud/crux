use std::fmt;
use std::ops;

use crate::object::{Object, ObjectType};

#[derive(Clone)]
pub(crate) enum Value {
    Boolean(bool),
    Number(f64),
    Null,
    Object(*mut Object)
}

impl Value {
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

    pub(crate) fn as_string(&self) -> Option<&str> {
	match self {
	    Self::Object(o) => unsafe {
		match (**o).object_type() {
		    ObjectType::String(s) => Some(s),
		    _ => None
		}
	    },
	    _ => None
	}
    }
}

impl fmt::Display for Value {
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
			ObjectType::String(s) => s.clone()
		    }
		}
	    }
	)
    }
}

impl PartialEq for Value {
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

impl ops::Add for Value {
    type Output = Result<Self, &'static str>;

    fn add(self, rhs: Self) -> Self::Output {
	match (self, rhs) {
	    (Self::Number(a), Self::Number(b)) => return Ok(Self::Number(a + b)),
	    _ => Err("Operands must be numbers.")
	}
    }
}

impl ops::Sub for Value {
    type Output = Result<Self, &'static str>;

    fn sub(self, rhs: Self) -> Self::Output {
	match (self, rhs) {
	    (Self::Number(a), Self::Number(b)) => Ok(Self::Number(a - b)),
	    _ => Err("Operands must be numbers.")
	}
    }
}

impl ops::Mul for Value {
    type Output = Result<Self, &'static str>;

    fn mul(self, rhs: Self) -> Self::Output {
	match (self, rhs) {
	    (Self::Number(a), Self::Number(b)) => Ok(Self::Number(a * b)),
	    _ => Err("Operands must be numbers.")
	}
    }
}

impl ops::Div for Value {
    type Output = Result<Self, &'static str>;

    fn div(self, rhs: Self) -> Self::Output {
	match (self, rhs) {
	    (Self::Number(a), Self::Number(b)) => Ok(Self::Number(a / b)),
	    _ => Err("Operands must be numbers.")
	}
    }
}
