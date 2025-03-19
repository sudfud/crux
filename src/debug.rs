use crate::chunk::{Chunk, Opcode};

/// Print each instruction in a Chunk to the console
pub(crate) fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);

    let mut offset: usize = 0;

    while offset < chunk.count() {
	offset = disassemble_instruction(chunk, offset);
    }
}

/// Print a single instruction at a given offset
pub(crate) fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("{:04} ", offset);

    // Print line information
    if offset > 0 && chunk.read_line(offset) == chunk.read_line(offset - 1) {
	print!("   | ");
    }
    else {
	print!("{:4} ", chunk.read_line(offset));
    }

    // Print instruction information
    let instruction: u8 = chunk.read_byte(offset);

    match Opcode::from_repr(instruction) {
	Some(opcode) => match opcode {
	    Opcode::Constant => constant_instruction("OP_CONSTANT", chunk, offset, false),
	    Opcode::ConstantLong => constant_instruction("OP_CONSTANT_LONG", chunk, offset, true),
	    Opcode::Add => simple_instruction("OP_ADD", offset),
	    Opcode::Subtract => simple_instruction("OP_SUBTRACT", offset),
	    Opcode::Multiply => simple_instruction("OP_MULTIPLY", offset),
	    Opcode::Divide => simple_instruction("OP_DIVIDE", offset),
	    Opcode::Negate => simple_instruction("OP_NEGATE", offset),
	    Opcode::Return => simple_instruction("OP_RETURN", offset)
	},
	None => {
	    println!("Unknown opcode {}", instruction);
	    offset + 1
	}
    }
}

/// Print the instruction name
fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 1
}

/// Print a constant-based instruction, including its index and value
fn constant_instruction(name: &str, chunk: &Chunk, offset: usize, long: bool) -> usize {
    let constant_index = if !long {
	chunk.read_byte(offset + 1) as usize
    } else {
	(chunk.read_byte(offset + 1) as usize) | ((chunk.read_byte(offset + 2) as usize) << 8)
    };

    print!("{:-16} {:4} '", name, constant_index);
    print!("{}", chunk.read_constant(constant_index));
    println!("'");

    if !long {
	offset + 2
    }
    else {
	offset + 3
    }
}

