use crate::chunk::{Chunk, Opcode};
use crate::vm::Globals;

/// Print each instruction in a Chunk to the console
#[allow(dead_code)]
pub(crate) fn disassemble_chunk(chunk: &Chunk, globals: &Globals, name: &str) {
    println!("== {} ==", name);

    let mut offset: usize = 0;

    while offset < chunk.count() {
	offset = disassemble_instruction(chunk, globals, offset);
    }
}

/// Print a single instruction at a given offset
#[allow(dead_code)]
pub(crate) fn disassemble_instruction(chunk: &Chunk, globals: &Globals, offset: usize) -> usize {
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
	    Opcode::Null => simple_instruction("OP_NULL", offset),
	    Opcode::True => simple_instruction("OP_TRUE", offset),
	    Opcode::False => simple_instruction("OP_FALSE", offset),
	    Opcode::Pop => simple_instruction("OP_POP", offset),
	    Opcode::GetLocal => byte_instruction("OP_GET_LOCAL", chunk, offset, false),
	    Opcode::GetLocalLong => byte_instruction("OP_GET_LOCAL_LONG", chunk, offset, true),
	    Opcode::SetLocal => byte_instruction("OP_SET_LOCAL", chunk, offset, false),
	    Opcode::SetLocalLong => byte_instruction("OP_SET_LOCAL_LONG", chunk, offset, true),
	    Opcode::GetGlobal => global_instruction("OP_GET_GLOBAL", chunk, globals, offset, false),
	    Opcode::GetGlobalLong => global_instruction("OP_GET_GLOBAL_LONG", chunk, globals, offset, true),
	    Opcode::DefineGlobal => global_instruction("OP_DEFINE_GLOBAL", chunk, globals, offset, false),
	    Opcode::DefineGlobalLong => global_instruction("OP_DEFINE_GLOBAL_LONG", chunk, globals, offset, true),
	    Opcode::SetGlobal => global_instruction("OP_SET_GLOBAL", chunk, globals, offset, false),
	    Opcode::SetGlobalLong => global_instruction("OP_SET_GLOBAL_LONG", chunk, globals, offset, true),
	    Opcode::Equal => simple_instruction("OP_EQUAL", offset),
	    Opcode::NotEqual => simple_instruction("OP_NOT_EQUAL", offset),
	    Opcode::Greater => simple_instruction("OP_GREATER", offset),
	    Opcode::GreaterEqual => simple_instruction("OP_GREATER_EQUAL", offset),
	    Opcode::Less => simple_instruction("OP_LESS", offset),
	    Opcode::LessEqual => simple_instruction("OP_LESS_EQUAL", offset),
	    Opcode::Add => simple_instruction("OP_ADD", offset),
	    Opcode::Subtract => simple_instruction("OP_SUBTRACT", offset),
	    Opcode::Multiply => simple_instruction("OP_MULTIPLY", offset),
	    Opcode::Divide => simple_instruction("OP_DIVIDE", offset),
	    Opcode::Not => simple_instruction("OP_NOT", offset),
	    Opcode::Negate => simple_instruction("OP_NEGATE", offset),
	    Opcode::Print => simple_instruction("OP_PRINT", offset),
	    Opcode::Jump => jump_instruction("OP_JUMP", 1, chunk, offset),
	    Opcode::JumpIfFalse => jump_instruction("OP_JUMP_IF_FALSE", 1, chunk, offset),
	    Opcode::Return => simple_instruction("OP_RETURN", offset)
	},
	None => {
	    println!("Unknown opcode {}", instruction);
	    offset + 1
	}
    }
}

/// Print the instruction name
#[allow(dead_code)]
fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 1
}

/// Print a constant-based instruction, including its index and value
#[allow(dead_code)]
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

#[allow(dead_code)]
fn byte_instruction(name: &str, chunk: &Chunk, offset: usize, long: bool) -> usize {
    let slot = if !long {
	chunk.read_byte(offset + 1) as usize
    } else {
	(chunk.read_byte(offset + 1) as usize) | ((chunk.read_byte(offset + 2) as usize) << 8)
    };

    println!("{:-16} {:4}", name, slot);

    if !long {
	offset + 2
    }
    else {
	offset + 3
    }
}

#[allow(dead_code)]
fn global_instruction(name: &str, chunk: &Chunk,  globals: &Globals, offset: usize, long: bool) -> usize {
    let global_index = if !long {
	chunk.read_byte(offset + 1) as usize
    } else {
	(chunk.read_byte(offset + 1) as usize) | ((chunk.read_byte(offset + 2) as usize) << 8)
    };

    print!("{:-16} {:4} '", name, global_index);
    print!("{}", globals.value(global_index));
    println!("'");

    if !long {
	offset + 2
    }
    else {
	offset + 3
    }
}

#[allow(dead_code)]
fn jump_instruction(name: &str, sign: isize, chunk: &Chunk, offset: usize) -> usize {
    let mut jump = chunk.read_byte(offset + 1) as usize;

    jump |= (chunk.read_byte(offset + 2) as usize) << 8;

    println!("{:-16} {:4} -> {}", name, offset, offset + 3 + (sign * jump as isize) as usize);

    offset + 3
}
