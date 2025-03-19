use std::cell::RefCell;

use crate::chunk::Chunk;
use crate::scanner::{Scanner, Token, TokenType};

struct Parser<'a> {
    current: Token<'a>,
    previous: Token<'a>
}

impl <'a> Parser<'a> {
    fn new() -> Self {
	Self {
	    current: Token::dummy(),
	    previous: Token::dummy()
	}
    }

    /// Report an error found at the current token
    fn error_at_current(&self, message: &str) {
	self.error_at(&self.previous, message);
    }

    /// Report an error found at the given token
    fn error_at(&self, token: &Token, message: &str) {
	
    }
}

struct Compiler<'a> {
    chunk: &'a mut Chunk,
    scanner: Scanner<'a>,
    parser: Parser<'a>
}

impl <'a> Compiler<'a> {
    fn new(source: &'a str, chunk: &'a mut Chunk) -> Self {
	Self {
	    chunk,
	    scanner: Scanner::new(source),
	    parser: Parser::new()
	}
    }

    /// Scan for the next token
    fn advance(&mut self) {
	self.parser.previous = self.parser.current;

	loop {
	    let token = self.scanner.scan_token();
	    
	    self.parser.current = token;

	    if self.parser.current.token_type() != TokenType::Error {
		break;
	    }

	    //self.error_at_current(self.parser.current.lexeme());
	}
    }
}

pub(crate) fn compile(source: &str, chunk: &mut Chunk) -> bool {
    let mut compiler = Compiler::new(source, chunk);

    compiler.advance();
    compiler.expression();
    compiler.consume(TokenType::EOF, "Expect end of expression.");

    false
}
