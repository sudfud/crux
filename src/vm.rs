use crate::chunk::{Chunk, Opcode};
use crate::compiler;
use crate::value::Value;

#[cfg(feature = "trace_execution")]
use crate::debug;

pub(crate) enum InterpretError {
    Compile,
    Runtime
}

pub(crate) struct VM {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>
}

impl VM {
    pub(crate) fn new() -> Self {
	Self {
	    chunk: Chunk::new(),
	    ip: 0,
	    stack: Vec::new()
	}
    }

    pub(crate) fn interpret(&mut self, source: &str) -> Result<(), InterpretError> {
	if !compiler::compile(source, &mut self.chunk) {
	    self.chunk.reset();
	    return Err(InterpretError::Compile);
	}

	self.ip = 0;

	let result = self.run();

	self.chunk.reset();

	result
    }

    pub(crate) fn free(&mut self) {
	self.ip = 0;
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

    fn binary_op(&mut self, op: fn(Value, Value) -> Value) -> Result<(), InterpretError> {
	let b = self.pop_value()?;
	let a = self.pop_value()?;
	
	self.push_value(op(a, b));

	Ok(())
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

	// Loop until Return or Error
	loop {
	    // Print debug info
	    #[cfg(feature = "trace_execution")] {
		print!("          ");
		for value in self.stack.iter() {
		    print!("[ {} ]", *value);
		}
		println!();
		debug::disassemble_instruction(&self.chunk, self.ip);
	    }

	    // Read and execute next instruction
	    match Opcode::from_repr(read_byte!()) {
		Some(instruction) => match instruction {
		    Opcode::Constant => {
			let constant: Value = read_constant!();
			self.push_value(constant);
		    },
		    Opcode::ConstantLong => {
			let constant: Value = read_constant_long!();
			self.push_value(constant);
			
		    },
		    Opcode::Add => self.binary_op(Value::add)?,
		    Opcode::Subtract => self.binary_op(Value::sub)?,
		    Opcode::Multiply => self.binary_op(Value::mul)?,
		    Opcode::Divide => self.binary_op(Value::div)?,
		    Opcode::Negate => match self.stack.last_mut() {
			Some(value) => *value = -*value,
			None => return Err(InterpretError::Runtime)
		    },
		    Opcode::Return => {
			println!("{}", self.pop_value()?);
			break;
		    },
		    _ => {}
		},
		None => return Err(InterpretError::Runtime)
	    }
	}
	
	Ok(())
    }
}
