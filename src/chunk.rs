use strum_macros::FromRepr;

use crate::value::Value;

#[derive(FromRepr, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum Opcode {
    Constant,
    ConstantLong,
    Null,
    True,
    False,
    Pop,
    GetGlobal,
    GetGlobalLong,
    DefineGlobal,
    DefineGlobalLong,
    SetGlobal,
    SetGlobalLong,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Print,
    Return
}

pub(crate) struct Chunk {
    code: Vec<u8>,
    lines: Vec<(usize, usize)>,
    constants: Vec<Value>
}

impl Chunk {
    pub(crate) fn new() -> Self {
	Self {
	    code: Vec::new(),
	    lines: Vec::new(),
	    constants: Vec::new()
	}
    }

    /// Return the number of bytes in this Chunk's bytecode
    pub(crate) fn count(&self) -> usize {
	self.code.len()
    }

    /// Read a byte from the bytecode at the given index
    pub(crate) fn read_byte(&self, index: usize) -> u8 {
	self.code[index]
    }

    /// Add a byte to the end of the bytecode
    pub(crate) fn write_byte(&mut self, byte: u8, line: usize) {
	self.code.push(byte);
	
	match self.lines.last_mut() {
	    Some((prev_line, count)) => if line == *prev_line {
		*count += 1;
	    } else {
		self.lines.push((line, 1));
	    },
	    None => self.lines.push((line, 1))
	}
    }

    /// Read the line number associated with the given bytecode index
    pub(crate) fn read_line(&self, index: usize) -> usize {
	let mut total = 0;

	for (line, count) in self.lines.iter() {
	    total += count;

	    if total > index {
		return *line;
	    }
	}

	self.lines[self.lines.len() - 1].0
    }

    /// Read a constant Value from the constant table
    pub(crate) fn read_constant(&self, index: usize) -> &Value {
	&self.constants[index]
    }

    /// Add a constant Value to the constants table
    pub(crate) fn add_constant(&mut self, value: Value) -> usize {
	self.constants.push(value);
	self.constants.len() - 1
    }

    /*
    /// Write a constant instruction and index to the bytecode
    pub(crate) fn write_constant(&mut self, value: Value, line: usize) {
	let constant_index = self.add_constant(value);

	if constant_index <= 0xFF {
	    self.write_byte(Opcode::Constant as u8, line);
	    self.write_byte(constant_index as u8, line);
	}
	else if constant_index <= 0xFFFF {
	    self.write_byte(Opcode::ConstantLong as u8, line);
	    self.write_byte((constant_index & 0x00FF) as u8, line);
	    self.write_byte(((constant_index & 0xFF00) >> 8) as u8, line);
	}
	else {
	    panic!("Too many constants in one chunk");
	}
    }
     */
    
    pub(crate) fn reset(&mut self) {
	self.code.clear();
	self.lines.clear();
	self.constants.clear();
    }
}
