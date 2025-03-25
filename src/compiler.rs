use std::fmt;
use std::collections::HashMap;

use strum_macros::FromRepr;

use crate::chunk::{Chunk, Opcode};
use crate::object::{Heap, Object};
use crate::scanner::{Scanner, Token, TokenType};
use crate::value::Value;

#[cfg(feature = "print_code")]
use crate::debug;

struct Parser<'a> {
    current: Token<'a>,
    previous: Token<'a>,
    error: Option<CompileError>,
    panic_mode: bool
}

impl <'a> Parser<'a> {
    fn new() -> Self {
	Self {
	    current: Token::dummy(),
	    previous: Token::dummy(),
	    error: None,
	    panic_mode: false
	}
    }
}

#[derive(FromRepr, PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
#[repr(u8)]
enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary
}

type ParseFn<'a, 'b> = fn(&mut Compiler<'a, 'b>) -> ();

struct ParseRule<'a, 'b> {
    prefix: Option<ParseFn<'a, 'b>>,
    infix: Option<ParseFn<'a, 'b>>,
    precedence: Precedence
}

impl <'a, 'b> ParseRule<'a, 'b> {
    fn new(prefix: Option<ParseFn<'a, 'b>>, infix: Option<ParseFn<'a, 'b>>, precedence: Precedence) -> Self {
	Self {
	    prefix,
	    infix,
	    precedence
	}
    }
}

#[derive(Debug)]
pub(crate) struct CompileError {
    message: String
}

impl CompileError {
    fn new(msg: &str, token: &Token) -> Self {
	let mut message = String::new();

	message.push_str(&format!("[line {}] Error", token.line()));

	if token.token_type() == TokenType::EOF {
	    message.push_str(" at end");
	}
	else if token.token_type() == TokenType::Error {
	    // Nothing
	}
	else {
	    message.push_str(&format!(" at '{:.*}'", token.lexeme().len(), token.lexeme()));
	}

	message.push_str(&format!(": {}", msg));
	
	Self { message }
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
	write!(f, "{}", self.message)
    }
}

impl std::error::Error for CompileError {}

struct Compiler<'a: 'b, 'b> {
    chunk: &'b mut Chunk<'a>,
    heap: &'b mut Heap<'a>,
    scanner: Scanner<'b>,
    parser: Parser<'b>,
    parse_rules: HashMap<TokenType, ParseRule<'a, 'b>>
}

impl <'a, 'b> Compiler<'a, 'b> {
    fn new(source: &'b str, chunk: &'b mut Chunk<'a>, heap: &'b mut Heap<'a>) -> Self {
	Self {
	    chunk,
	    heap,
	    scanner: Scanner::new(source),
	    parser: Parser::new(),
	    parse_rules: HashMap::from([
		(TokenType::LeftParen, ParseRule::new(Some(Compiler::grouping), None, Precedence::None)),
		(TokenType::RightParen, ParseRule::new(None, None, Precedence::None)),
		(TokenType::LeftBrace, ParseRule::new(None, None, Precedence::None)),
		(TokenType::RightBrace, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Comma, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Dot, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Minus, ParseRule::new(Some(Compiler::unary), Some(Compiler::binary), Precedence::Term)),
		(TokenType::Plus, ParseRule::new(None, Some(Compiler::binary), Precedence::Term)),
		(TokenType::Semicolon, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Slash, ParseRule::new(None, Some(Compiler::binary), Precedence::Factor)),
		(TokenType::Star, ParseRule::new(None, Some(Compiler::binary), Precedence::Factor)),
		(TokenType::Bang, ParseRule::new(Some(Compiler::unary), None, Precedence::None)),
		(TokenType::BangEqual, ParseRule::new(None, Some(Compiler::binary), Precedence::Equality)),
		(TokenType::Equal, ParseRule::new(None, None, Precedence::None)),
		(TokenType::EqualEqual, ParseRule::new(None, Some(Compiler::binary), Precedence::Equality)),
		(TokenType::Greater, ParseRule::new(None, Some(Compiler::binary), Precedence::Comparison)),
		(TokenType::GreaterEqual, ParseRule::new(None, Some(Compiler::binary), Precedence::Comparison)),
		(TokenType::Less, ParseRule::new(None, Some(Compiler::binary), Precedence::Comparison)),
		(TokenType::LessEqual, ParseRule::new(None, Some(Compiler::binary), Precedence::Comparison)),
		(TokenType::Identifier, ParseRule::new(None, None, Precedence::None)),
		(TokenType::String, ParseRule::new(Some(Compiler::string), None, Precedence::None)),
		(TokenType::Number, ParseRule::new(Some(Compiler::number), None, Precedence::None)),
		(TokenType::And, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Class, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Else, ParseRule::new(None, None, Precedence::None)),
		(TokenType::False, ParseRule::new(Some(Compiler::literal), None, Precedence::None)),
		(TokenType::For, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Function, ParseRule::new(None, None, Precedence::None)),
		(TokenType::If, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Null, ParseRule::new(Some(Compiler::literal), None, Precedence::None)),
		(TokenType::Or, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Print, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Return, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Super, ParseRule::new(None, None, Precedence::None)),
		(TokenType::This, ParseRule::new(None, None, Precedence::None)),
		(TokenType::True, ParseRule::new(Some(Compiler::literal), None, Precedence::None)),
		(TokenType::Var, ParseRule::new(None, None, Precedence::None)),
		(TokenType::While, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Error, ParseRule::new(None, None, Precedence::None)),
		(TokenType::EOF, ParseRule::new(None, None, Precedence::None)),
	    ])
	}
    }

    fn current_chunk(&mut self) -> &mut Chunk<'a> {
	self.chunk
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

	    self.error_at_current(self.parser.current.lexeme());
	}
    }

    /// Check if the parser's current token matches the given type
    fn consume(&mut self, token_type: TokenType, message: &'static str) {
	if self.parser.current.token_type() == token_type {
	    self.advance();
	    return;
	}

	self.error_at_current(message);
    }

    /// Write a single byte to the current chunk
    fn emit_byte(&mut self, byte: u8) {
	let line = self.parser.previous.line();
	self.current_chunk().write_byte(byte, line);
    }

    /// Write two bytes to the current chunk
    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
	self.emit_byte(byte1);
	self.emit_byte(byte2);
    }

    /// Write return opcode to the current chunk
    fn emit_return(&mut self) {
	self.emit_byte(Opcode::Return as u8);
    }

    /// Write a constant to the chunk's constant table
    fn emit_constant(&mut self, value: Value<'a>) {
	let constant_index = self.current_chunk().add_constant(value);

	if constant_index <= 0xFF {
	    self.emit_bytes(Opcode::Constant as u8, constant_index as u8);
	}
	else if constant_index <= 0xFFFF {
	    self.emit_byte(Opcode::ConstantLong as u8);
	    self.emit_bytes((constant_index & 0x00FF) as u8, ((constant_index & 0xFF00) >> 8) as u8);
	}
	else {
	    self.error("Too many constants in one chunk.");
	}
    }

    /// End compilation
    fn end(&mut self) {
	self.emit_return();

	#[cfg(feature = "print_code")]
	if !self.parser.had_error {
	    debug::disassemble_chunk(self.current_chunk(), "code");
	}
    }

    /// Compile a binary expression
    fn binary(&mut self) {
	let op_type = self.parser.previous.token_type();
	let rule = &self.parse_rules[&op_type];
	let next_prec = Precedence::from_repr(rule.precedence as u8 + 1)
	    .unwrap_or(Precedence::Primary);

	self.parse_precedence(next_prec);

	match op_type {
	    TokenType::BangEqual => self.emit_byte(Opcode::NotEqual as u8),
	    TokenType::EqualEqual => self.emit_byte(Opcode::Equal as u8),
	    TokenType::Greater => self.emit_byte(Opcode::Greater as u8),
	    TokenType::GreaterEqual => self.emit_byte(Opcode::GreaterEqual as u8),
	    TokenType::Less => self.emit_byte(Opcode::Less as u8),
	    TokenType::LessEqual => self.emit_byte(Opcode::LessEqual as u8),
	    TokenType::Plus => self.emit_byte(Opcode::Add as u8),
	    TokenType::Minus => self.emit_byte(Opcode::Subtract as u8),
	    TokenType::Star => self.emit_byte(Opcode::Multiply as u8),
	    TokenType::Slash => self.emit_byte(Opcode::Divide as u8),
	    _ => self.error("Invalid token for binary expression.")
	}
    }

    /// Compile a literal expression
    fn literal(&mut self) {
	match self.parser.previous.token_type() {
	    TokenType::False => self.emit_byte(Opcode::False as u8),
	    TokenType::Null => self.emit_byte(Opcode::Null as u8),
	    TokenType::True => self.emit_byte(Opcode::True as u8),
	    _ => self.error("Invalid token for literal expression.")
	}
    }

    /// Compile a grouped expression
    fn grouping(&mut self) {
	self.expression();
	self.consume(TokenType::RightParen, "Expect ')' after expression");
    }

    /// Compile a number literal
    fn number(&mut self) {
	match self.parser.previous.lexeme().parse::<f64>() {
	    Ok(value) => self.emit_constant(Value::Number(value)),
	    Err(e) => self.error(&e.to_string())
	}
    }

    /// Compile a string literal
    fn string(&mut self) {
	let text = self.parser.previous.lexeme();
	let string = self.heap.allocate_string(&text[1..text.len() - 1]);
	self.emit_constant(Value::Object(string));
    }

    /// Compile a unary expression
    fn unary(&mut self) {
	let op_type: TokenType = self.parser.previous.token_type();

	self.parse_precedence(Precedence::Unary);

	match op_type {
	    TokenType::Bang => self.emit_byte(Opcode::Not as u8),
	    TokenType::Minus => self.emit_byte(Opcode::Negate as u8),
	    // _ => unreachable!("Invalid token for unary expression")
	    _ => self.error("Invalid token for unary expression")
	}
    }

    /// Parse any expression with the given precedence or higher
    fn parse_precedence(&mut self, precedence: Precedence) {
	self.advance();

	let prefix_rule = &self.parse_rules[&self.parser.previous.token_type()].prefix;

	match prefix_rule {
	    Some(prefix) => prefix(self),
	    None => self.error("Expect expression.")
	}

	while precedence <= self.parse_rules[&self.parser.current.token_type()].precedence {
	    self.advance();
	    let infix_rule = &self.parse_rules[&self.parser.previous.token_type()].infix;
	    if let Some(infix) = infix_rule {
		infix(self);
	    }
	}
    }

    fn get_rule(&self, token_type: TokenType) -> &'a ParseRule {
	&self.parse_rules[&token_type]
    }

    /// Compile an expression
    fn expression(&mut self) {
	self.parse_precedence(Precedence::Assignment);
    }

    /// Report an error found at the previous token
    fn error(&mut self, message: &str) {
	self.error_at(self.parser.previous, message);
    }
    
    /// Report an error found at the current token
    fn error_at_current(&mut self, message: &str) {
	self.error_at(self.parser.current, message);
    }

    /// Report an error found at the given token
    fn error_at(&mut self, token: Token<'b>, message: &str) {
	if self.parser.panic_mode {
	    return;
	}

	self.parser.panic_mode = true;

	/*
	eprint!("[line {}] Error", token.line());

	if token.token_type() == TokenType::EOF {
	    eprint!(" at end");
	}
	else if token.token_type() == TokenType::Error {
	    // Nothing
	}
	else {
	    eprint!(" at '{:.*}'", token.lexeme().len(), token.lexeme());
	}

	eprintln!(": {}", message);
	 */
	
	self.parser.error = Some(CompileError::new(message, &token));
    }
}

pub(crate) fn compile<'a>(source: &str, chunk: &mut Chunk<'a>, heap: &mut Heap<'a>) -> Result<(), CompileError> {
    let mut compiler = Compiler::new(source, chunk, heap);

    compiler.advance();
    compiler.expression();
    compiler.consume(TokenType::EOF, "Expect end of expression.");

    compiler.end();

    if let Some(error) = compiler.parser.error.take() {
	Err(error)
    }
    else {
	Ok(())
    }
}
