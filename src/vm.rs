use std::collections::HashMap;

use crate::chunk::{Chunk, Opcode};
use crate::compiler::{self, CompileError};
use crate::object::Heap;
use crate::value::Value;

#[cfg(feature = "trace_execution")]
use crate::debug;

pub(crate) struct Globals {
    names: HashMap<String, usize>,
    values: Vec<Value>
}

impl Globals {
    fn new() -> Self {
	Self {
	    names: HashMap::new(),
	    values: Vec::new()
	}
    }

    pub(crate) fn contains(&self, name: &str) -> bool {
	self.names.contains_key(name)
    }

    pub(crate) fn index(&self, name: &str) -> Option<usize> {
	self.names
	    .get(name)
	    .map(|idx| *idx)
    }

    pub(crate) fn value(&self, index: usize) -> &Value {
	&self.values[index]
    }

    pub(crate) fn insert(&mut self, name: String, value: Value) -> usize {
	self.values.push(value);

	let index = self.values.len() - 1;
	self.names.insert(name, index);
	index
    }
}

pub(crate) enum InterpretError {
    Compile(CompileError),
    Runtime
}

pub(crate) struct VM {
    chunk: Chunk,
    globals: Globals,
    heap: Heap,
    ip: usize,
    stack: Vec<Value>
}

impl VM {
    pub(crate) fn new() -> Self {
	Self {
	    chunk: Chunk::new(),
	    globals: Globals::new(),
	    heap: Heap::new(),
	    ip: 0,
	    stack: Vec::new()
	}
    }

    pub(crate) fn interpret(&mut self, source: &str) -> Result<(), InterpretError> {
	let result = compiler::compile(source, &mut self.chunk, &mut self.heap, &mut self.globals);
	
	if let Err(e) = result {
	    self.chunk.reset();
	    return Err(InterpretError::Compile(e));
	}

	self.ip = 0;

	let result = self.run();

	self.chunk.reset();

	result
    }

    pub(crate) fn free(&mut self) {
	self.heap.free();
	self.ip = 0;
	self.stack.clear();
    }

    fn runtime_error(&mut self, message: String) {
	eprintln!("{}", message);

	let line = self.chunk.read_line(self.ip);

	eprintln!("[line {}] in script", line);

	self.stack.clear();
    }

    fn push_value(&mut self, value: Value) {
	self.stack.push(value)
    }

    fn pop_value(&mut self) -> Result<Value, InterpretError> {
	self.stack
	    .pop()
	    .ok_or(InterpretError::Runtime)
    }

    fn peek_value(&self, distance: usize) -> &Value {
	&self.stack[self.stack.len() - 1 - distance]
    }

    fn binary_op(&mut self, op: fn(Value, Value) -> Result<Value, &'static str>) -> Result<(), InterpretError> {
	match op(self.peek_value(1).clone(), self.peek_value(0).clone()) {
	    Ok(value) => {
		self.pop_value()?;
		self.pop_value()?;
		self.push_value(value);

		Ok(())
	    },
	    Err(e) => {
		self.runtime_error(e.into());
		Err(InterpretError::Runtime)
	    }
	}
    }

    fn comparison(&mut self, op: fn(&f64, &f64) -> bool) -> Result<(), InterpretError> {
	match (self.peek_value(1), self.peek_value(0)) {
	    (Value::Number(n1), Value::Number(n2)) => {
		self.push_value(Value::Boolean(op(n1, n2)));
		Ok(())
	    },
	    _ => {
		self.runtime_error("Operands must be numbers.".into());
		Err(InterpretError::Runtime)
	    }
	}
    }

    fn run(&mut self) -> Result<(), InterpretError> {
	use std::ops::{Add, Sub, Mul, Div};
	
	// Define macros
	macro_rules! read_byte {
	    () => {
		{
		    let byte = self.chunk.read_byte(self.ip);

		    self.ip += 1;

		    byte
		}
	    };
	}

	macro_rules! read_word {
	    () => {
		{
		    let mut word = read_byte!() as usize;
		    word |= (read_byte!() as usize) << 8;

		    word
		}
	    };
	}

	macro_rules! read_constant {
	    () => {
		self.chunk.read_constant(read_byte!() as usize)
	    };
	}

	macro_rules! read_constant_long {
	    () => {
		self.chunk
		    .read_constant(read_byte!() as usize | ((read_byte!() as usize) << 8))
	    };
	}

	macro_rules! read_string {
	    () => {
		read_constant!().as_string().unwrap()
	    };
	}

	macro_rules! read_string_long {
	    () => {
		read_constant_long!().as_string().unwrap()
	    };
	}

	// Loop until Return or Error
	loop {
	    // Print debug info
	    #[cfg(feature = "trace_execution")] {
		print!("          ");
		for value in self.stack.iter() {
		    print!("[ {} ]", *value);
		}
		println!();
		debug::disassemble_instruction(&self.chunk, &self.globals, self.ip);
	    }

	    // Read and execute next instruction
	    match Opcode::from_repr(read_byte!()) {
		Some(instruction) => match instruction {
		    Opcode::Constant => {
			let constant: &Value = read_constant!();
			self.push_value(constant.clone());
		    },
		    Opcode::ConstantLong => {
			let constant: &Value = read_constant_long!();
			self.push_value(constant.clone());
			
		    },
		    Opcode::Null => self.push_value(Value::Null),
		    Opcode::True => self.push_value(Value::Boolean(true)),
		    Opcode::False => self.push_value(Value::Boolean(false)),
		    Opcode::Pop => { self.pop_value()?; }
		    Opcode::GetLocal | Opcode::GetLocalLong => {
			let slot = if instruction == Opcode::GetLocal {
			    read_byte!() as usize
			} else {
			    read_word!()
			};

			let local = self.stack[slot].clone();

			self.push_value(local);
		    },
		    Opcode::SetLocal | Opcode::SetLocalLong => {
			let slot = if instruction == Opcode::SetLocal {
			    read_byte!() as usize
			} else {
			    read_word!()
			};

			let local = self.stack[slot].clone();

			self.push_value(local);
		    },
		    Opcode::GetGlobal | Opcode::GetGlobalLong => {
			let global_index = if instruction == Opcode::GetGlobal {
			    read_byte!() as usize
			} else {
			    read_word!()
			};

			let value = self.globals.value(global_index);

			if value.is_undefined() {
			    self.runtime_error("Undefined variable.".into());
			    return Err(InterpretError::Runtime);
			}

			self.push_value(value.clone());
		    },
		    Opcode::DefineGlobal | Opcode::DefineGlobalLong => {
			let global_index = if instruction == Opcode::DefineGlobal {
			    read_byte!() as usize
			} else {
			    read_word!()
			};

			self.globals.values[global_index] = self.pop_value()?;
		    },
		    Opcode::SetGlobal | Opcode::SetGlobalLong => {
			let global_index = if instruction == Opcode::SetGlobal {
			    read_byte!() as usize
			} else {
			    read_word!()
			};

			let value = self.globals.value(global_index);

			if value.is_undefined() {
			    self.runtime_error("Undefined variable.".into());
			    return Err(InterpretError::Runtime);
			}

			self.globals.values[global_index] = self.peek_value(0).clone();
		    },
		    Opcode::Equal => {
			let b = self.pop_value()?;
			let a = self.pop_value()?;

			self.push_value(Value::Boolean(a == b));
		    },
		    Opcode::NotEqual => {
			let b = self.pop_value()?;
			let a = self.pop_value()?;

			self.push_value(Value::Boolean(a != b));
		    },
		    Opcode::Greater => self.comparison(f64::gt)?,
		    Opcode::GreaterEqual => self.comparison(f64::ge)?,
		    Opcode::Less => self.comparison(f64::lt)?,
		    Opcode::LessEqual => self.comparison(f64::le)?,
		    Opcode::Add => {
			let (a, b) = (self.peek_value(1), self.peek_value(0));
			
			if let (Some(a), Some(b)) = (a.as_string(), b.as_string()) {
			
			let mut concat = String::from(a);
			concat.push_str(&b);

			let value = Value::Object(self.heap.allocate_string(concat));

			self.pop_value()?;
			self.pop_value()?;
			
			self.push_value(value);
		    } else if let (Some(a), Some(b)) = (a.as_number(), b.as_number()) {
			self.pop_value()?;
			self.pop_value()?;

			self.push_value(Value::Number(a + b));
		    } else {
			self.runtime_error("Operands must be numbers or strings".into());
			}
		    },
		    Opcode::Subtract => self.binary_op(Value::sub)?,
		    Opcode::Multiply => self.binary_op(Value::mul)?,
		    Opcode::Divide => self.binary_op(Value::div)?,
		    Opcode::Not => match self.stack.last_mut() {
			Some(value) => *value = Value::Boolean(value.is_falsey()),
			None => {
			    self.runtime_error("Attempt to access empty stack.".into());
			    return Err(InterpretError::Runtime);
			}
		    },
		    Opcode::Negate => match self.stack.last_mut() {
			Some(value) => if let Value::Number(n) = value {
			    *value = Value::Number(-*n);
			} else {
			    self.runtime_error("Operand must be a number.".into());
			    return Err(InterpretError::Runtime);
			},
			None => {
			    self.runtime_error("Attempt to access empty stack.".into());
			    return Err(InterpretError::Runtime);
			}
		    },
		    Opcode::Print => {
			println!("{}", self.pop_value()?);
		    },
		    Opcode::Return => return Ok(())
		},
		None => return Err(InterpretError::Runtime)
	    }
	}
	
	Ok(())
    }
}
