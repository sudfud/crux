mod chunk;
mod compiler;
mod debug;
mod object;
mod scanner;
mod value;
mod vm;

use chunk::{Chunk, Opcode};
use vm::{VM, InterpretError};

fn repl(vm: &mut VM) {
    use std::io::{self, Write};
    
    loop {
	let mut input = String::new();
	
	print!("> ");

	io::stdout().flush();

	match io::stdin().read_line(&mut input) {
	    Ok(_) => if input.trim() == "exit" {
		return;
	    } else if let Err(e) = vm.interpret(&input) {
		match e {
		    InterpretError::Compile(e) => eprintln!("Compile Error: {}", e),
		    InterpretError::Runtime => eprintln!("Runtime Error")
		}
	    },
	    Err(e) => eprintln!("Failed to read user input: {}", e)
	}
    }
}

fn run_file(vm: &mut VM, path: &str) {
    match std::fs::read_to_string(path) {
	Ok(source) => if let Err(e) = vm.interpret(&source) {
	    match e {
		InterpretError::Compile(e) => eprintln!("Compile Error: {}", e),
		InterpretError::Runtime => eprintln!("Runtime Error")
	    }  
	},
	Err(e) => eprintln!("Failed to read file '{}': {}", path, e)
    }
}

fn main() {
    let mut vm = VM::new();

    let args: Vec<String> = std::env::args().collect();

    match args.len() {
	1 => repl(&mut vm),
	2 => run_file(&mut vm, &args[1]),
	_ => eprintln!("Usage: crux [path]")
    }
    
    vm.free();
}
