use std::fmt;
use std::collections::HashMap;

use strum_macros::FromRepr;

use crate::chunk::{Chunk, Opcode};
use crate::object::{Heap, Object};
use crate::scanner::{Scanner, Token, TokenType};
use crate::value::Value;
use crate::vm::Globals;

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

type ParseFn<'a> = fn(&mut Compiler<'a>, bool) -> ();

struct ParseRule<'a> {
    prefix: Option<ParseFn<'a>>,
    infix: Option<ParseFn<'a>>,
    precedence: Precedence
}

impl <'a> ParseRule<'a> {
    fn new(prefix: Option<ParseFn<'a>>, infix: Option<ParseFn<'a>>, precedence: Precedence) -> Self {
	Self {
	    prefix,
	    infix,
	    precedence
	}
    }
}

struct Local<'a> {
    name: Token<'a>,
    depth: usize
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

struct Compiler<'a> {
    chunk: &'a mut Chunk,
    heap: &'a mut Heap,
    globals: &'a mut Globals,
    locals: Vec<Local<'a>>,
    local_count: usize,
    scope_depth: usize,
    scanner: Scanner<'a>,
    parser: Parser<'a>,
    parse_rules: HashMap<TokenType, ParseRule<'a>>
}

impl <'a> Compiler<'a> {
    fn new(source: &'a str, chunk: &'a mut Chunk, heap: &'a mut Heap, globals: &'a mut Globals) -> Self {
	Self {
	    chunk,
	    heap,
	    globals,
	    locals: Vec::new(),
	    local_count: 0,
	    scope_depth: 0,
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
		(TokenType::Percent, ParseRule::new(None, Some(Compiler::binary), Precedence::Factor)),
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
		(TokenType::Identifier, ParseRule::new(Some(Compiler::variable), None, Precedence::None)),
		(TokenType::String, ParseRule::new(Some(Compiler::string), None, Precedence::None)),
		(TokenType::Number, ParseRule::new(Some(Compiler::number), None, Precedence::None)),
		(TokenType::And, ParseRule::new(None, Some(Compiler::and), Precedence::And)),
		(TokenType::Class, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Else, ParseRule::new(None, None, Precedence::None)),
		(TokenType::False, ParseRule::new(Some(Compiler::literal), None, Precedence::None)),
		(TokenType::For, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Function, ParseRule::new(None, None, Precedence::None)),
		(TokenType::If, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Null, ParseRule::new(Some(Compiler::literal), None, Precedence::None)),
		(TokenType::Or, ParseRule::new(None, Some(Compiler::or), Precedence::Or)),
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

    fn current_chunk(&self) -> &Chunk {
	self.chunk
    }

    fn current_chunk_mut(&mut self) -> &mut Chunk {
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

    /// Check if the current token matches the given type
    fn check_token(&self, token_type: TokenType) -> bool {
	self.parser.current.token_type() == token_type
    }

    /// Advance to the next token if the current one matches the given type
    fn match_token(&mut self, token_type: TokenType) -> bool {
	if !self.check_token(token_type) {
	    return false;
	}

	self.advance();
	true
    }

    /// Write a single byte to the current chunk
    fn emit_byte(&mut self, byte: u8) {
	let line = self.parser.previous.line();
	self.current_chunk_mut().push_byte(byte, line);
    }

    /// Write two bytes to the current chunk
    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
	self.emit_byte(byte1);
	self.emit_byte(byte2);
    }

    /// Write instructions for a loop to the current chunk
    fn emit_loop(&mut self, loop_start: usize) {
	self.emit_byte(Opcode::Loop as u8);

	let offset = self.current_chunk().count() - loop_start + 2;

	if offset > 0xFFFF {
	    self.error("Loop body too large.");
	}

	self.emit_bytes((offset & 0xFF) as u8, ((offset & 0xFF00) >> 8) as u8);
    }

    /// Write a jump address to the current chunk
    fn emit_jump(&mut self, instruction: u8) -> usize {
	self.emit_byte(instruction);
	self.emit_bytes(0xFF, 0xFF);

	self.current_chunk().count() - 2
    }

    /// Write return opcode to the current chunk
    fn emit_return(&mut self) {
	self.emit_byte(Opcode::Return as u8);
    }

    fn make_constant(&mut self, value: Value) -> usize {
	let constant_index = self.current_chunk_mut().add_constant(value);

	if constant_index <= 0xFFFF {
	    constant_index
	}
	else {
	    self.error("Too many constants in one chunk.");
	    0
	}
    }

    /// Write a constant to the chunk's constant table
    fn emit_constant(&mut self, value: Value) {
	let constant_index = self.make_constant(value);
	
	if constant_index <= 0xFF {
	    self.emit_bytes(Opcode::Constant as u8, constant_index as u8);
	}
	else if constant_index <= 0xFFFF {
	    self.emit_byte(Opcode::ConstantLong as u8);
	    self.emit_bytes((constant_index & 0x00FF) as u8, ((constant_index & 0xFF00) >> 8) as u8);
	}
    }

    fn patch_jump(&mut self, offset: usize) {
	let jump = self.current_chunk().count() - offset - 2;

	if jump > 0xFFFF {
	    self.error("Too much code to jump over.");
	}

	self.current_chunk_mut().write_byte(offset, (jump & 0xFF) as u8);
	self.current_chunk_mut().write_byte(offset + 1, ((jump & 0xFF00) >> 8) as u8);
    }

    /// End compilation
    fn end(&mut self) {
	self.emit_return();

	#[cfg(feature = "print_code")]
	if self.parser.error.is_none() {
	    debug::disassemble_chunk(self.current_chunk(), self.globals, "code");
	}
    }

    /// Enter a local scope
    fn begin_scope(&mut self) {
	self.scope_depth += 1;
    }

    /// Exit a local scope
    fn end_scope(&mut self) {
	self.scope_depth -= 1;

	while self.locals.len() > 0 && self.locals[self.locals.len() - 1].depth > self.scope_depth {
	    self.emit_byte(Opcode::Pop as u8);
	    self.locals.pop();
	}
    }

    /// Compile a binary expression
    fn binary(&mut self, can_assign: bool) {
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
	    TokenType::Percent => self.emit_byte(Opcode::Modulo as u8),
	    TokenType::Minus => self.emit_byte(Opcode::Subtract as u8),
	    TokenType::Star => self.emit_byte(Opcode::Multiply as u8),
	    TokenType::Slash => self.emit_byte(Opcode::Divide as u8),
	    _ => self.error("Invalid token for binary expression.")
	}
    }

    /// Compile a literal expression
    fn literal(&mut self, can_assign: bool) {
	match self.parser.previous.token_type() {
	    TokenType::False => self.emit_byte(Opcode::False as u8),
	    TokenType::Null => self.emit_byte(Opcode::Null as u8),
	    TokenType::True => self.emit_byte(Opcode::True as u8),
	    _ => self.error("Invalid token for literal expression.")
	}
    }

    /// Compile a grouped expression
    fn grouping(&mut self, can_assign: bool) {
	self.expression();
	self.consume(TokenType::RightParen, "Expect ')' after expression");
    }

    /// Compile a number literal
    fn number(&mut self, can_assign: bool) {
	match self.parser.previous.lexeme().parse::<f64>() {
	    Ok(value) => self.emit_constant(Value::Number(value)),
	    Err(e) => self.error(&e.to_string())
	}
    }

    /// Compile a logical or expression
    fn or(&mut self, can_assign: bool) {
	let else_jump = self.emit_jump(Opcode::JumpIfFalse as u8);
	let end_jump = self.emit_jump(Opcode::Jump as u8);

	self.patch_jump(else_jump);
	self.emit_byte(Opcode::Pop as u8);

	self.parse_precedence(Precedence::Or);
	self.patch_jump(end_jump);
    }

    /// Compile a string literal
    fn string(&mut self, can_assign: bool) {
	let text = self.parser.previous.lexeme();
	let string = self.heap.allocate_string((&text[1..text.len() - 1]).into());
	self.emit_constant(Value::Object(string));
    }

    fn named_variable(&mut self, name: String, can_assign: bool) {
	let (get_op, set_op, index): (Opcode, Opcode, usize) = {
	    if let Some(arg) = self.resolve_local(&name) {
		(
		    if arg <= 0xFF {Opcode::GetLocal} else {Opcode::GetLocalLong},
		    if arg <= 0xFF {Opcode::SetLocal} else {Opcode::SetLocalLong},
		    arg
		)
	    }
	    else {
		let arg = self.identifier_constant(&name);
		(
		    if arg <= 0xFF {Opcode::GetGlobal} else {Opcode::GetGlobalLong},
		    if arg <= 0xFF {Opcode::SetGlobal} else {Opcode::SetGlobalLong},
		    arg
		)
	    }
	};

	if can_assign && self.match_token(TokenType::Equal) {
	    self.expression();
	    
	    if set_op == Opcode::SetLocal || set_op == Opcode::SetGlobal {
		self.emit_bytes(set_op as u8, index as u8);
	    }
	    else {
		self.emit_byte(set_op as u8);
		self.emit_bytes((index & 0xFF) as u8, (index >> 8) as u8);
	    }
	}
	else {
	    if get_op == Opcode::GetLocal || get_op == Opcode::GetGlobal {
		self.emit_bytes(get_op as u8, index as u8);
	    }
	    else {
		self.emit_byte(get_op as u8);
		self.emit_bytes((index & 0xFF) as u8, (index >> 8) as u8);
	    }
	}
    }

    /// Compile a variable
    fn variable(&mut self, can_assign: bool) {
	let name = String::from(self.parser.previous.lexeme());
	self.named_variable(name, can_assign);
    }

    /// Compile a unary expression
    fn unary(&mut self, can_assign: bool) {
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
	let can_assign = precedence <= Precedence::Assignment;

	match prefix_rule {
	    Some(prefix) => prefix(self, can_assign),
	    None => self.error("Expect expression.")
	}

	while precedence <= self.parse_rules[&self.parser.current.token_type()].precedence {
	    self.advance();
	    let infix_rule = &self.parse_rules[&self.parser.previous.token_type()].infix;
	    
	    if let Some(infix) = infix_rule {
		infix(self, can_assign);
	    }

	    if can_assign && self.match_token(TokenType::Equal) {
		self.error("Invalid assignment target.");
	    }
	}
    }

    /// Write a variable identifier constant to the current chunk
    fn identifier_constant(&mut self, name: &str) -> usize {
	match self.globals.index(name) {
	    Some(idx) => idx,
	    None => self.globals.insert(String::from(name), Value::Undefined)
	}
    }

    /// Find the index of a local variable, return None if variable is global
    fn resolve_local(&mut self, name: &str) -> Option<usize> {
	for i in (0..self.locals.len()).rev() {
	    let local = &self.locals[i];

	    if local.name.lexeme() == name {
		if local.depth == 0 {
		    self.error("Can't read local variable in its own initializer.");
		}
		return Some(i);
	    }
	}

	None
    }

    /// Add a local variable to a compiler's local array
    fn add_local(&mut self, name: Token<'a>) {
	let local = Local {
	    name,
	    depth: 0
	};

	self.locals.push(local);
    }

    /// Declare a local variable
    fn declare_variable(&mut self) {
	if self.scope_depth == 0 {
	    return;
	}

	let name = self.parser.previous.clone();

	for i in (0..self.locals.len()).rev() {
	    let local = &self.locals[i];

	    if local.depth != 0 && local.depth < self.scope_depth {
		break;
	    }

	    if &name == &local.name {
		self.error("Already a variable with this name in scope.");
	    }
	}

	self.add_local(name);
    }

    fn parse_variable(&mut self, error_message: &'static str) -> usize {
	self.consume(TokenType::Identifier, error_message);

	self.declare_variable();
	
	if self.scope_depth > 0 {
	    return 0;
	}
	
	let name = self.parser.previous.lexeme();
	
	self.identifier_constant(name)
    }

    /// Initialize a declared local variable
    fn mark_initialized(&mut self) {
	let count = self.locals.len();
	self.locals[count - 1].depth = self.scope_depth;
    }

    /// Write instructions to define a global or local variable
    fn define_variable(&mut self, global_index: usize) {
	if self.scope_depth > 0 {
	    self.mark_initialized();
	    return;
	}
	
	if global_index <= 0xFF {
	    self.emit_bytes(Opcode::DefineGlobal as u8, global_index as u8);
	}
	else {
	    self.emit_byte(Opcode::DefineGlobalLong as u8);
	    self.emit_bytes((global_index & 0x00FF) as u8, (global_index >> 8) as u8);
	}
    }

    /// Compile a logical and operation
    fn and(&mut self, can_assign: bool) {
	let end_jump = self.emit_jump(Opcode::JumpIfFalse as u8);

	self.emit_byte(Opcode::Pop as u8);
	self.parse_precedence(Precedence::And);

	self.patch_jump(end_jump);
    }

    /// Compile an expression
    fn expression(&mut self) {
	self.parse_precedence(Precedence::Assignment);
    }

    /// Compile a block surrounded by curly braces
    fn block(&mut self) {
	while !self.check_token(TokenType::RightBrace) && !self.check_token(TokenType::EOF) {
	    self.declaration();
	}

	self.consume(TokenType::RightBrace, "Expect '}' after block.");
    }

    /// Compile a variable declaration
    fn var_declaration(&mut self) {
	let global_index = self.parse_variable("Expect variable name.");

	if self.match_token(TokenType::Equal) {
	    self.expression();
	}
	else {
	    self.emit_byte(Opcode::Null as u8);
	}

	self.consume(TokenType::Semicolon, "Expect ';' after variable declaration.");
	self.define_variable(global_index);
    }

    /// Compile an expression statement
    fn expression_statement(&mut self) {
	self.expression();
	self.consume(TokenType::Semicolon, "Expect ';' after expression.");
	self.emit_byte(Opcode::Pop as u8);
    }

    /// Compile a for loop
    fn for_statement(&mut self) {
	self.begin_scope();
	self.consume(TokenType::LeftParen, "Expect '(' after 'for'.");

	if self.match_token(TokenType::Semicolon) {
	    // No initializer
	}
	else if self.match_token(TokenType::Var) {
	    self.var_declaration();
	}
	else {
	    self.expression_statement();
	}

	let mut loop_start = self.current_chunk().count();
	let mut exit_jump: Option<usize> = None;
	
	if !self.match_token(TokenType::Semicolon) {
	    self.expression();
	    self.consume(TokenType::Semicolon, "Expect ';' after loop condition.");

	    exit_jump = Some(self.emit_jump(Opcode::JumpIfFalse as u8));
	    
	    self.emit_byte(Opcode::Pop as u8);
	}

	if !self.match_token(TokenType::RightParen) {
	    let body_jump = self.emit_jump(Opcode::Jump as u8);
	    let inc_start = self.current_chunk().count();

	    self.expression();
	    self.emit_byte(Opcode::Pop as u8);
	    self.consume(TokenType::RightParen, "Expect ')' after for clauses.");
	    self.emit_loop(loop_start);
	    
	    loop_start = inc_start;

	    self.patch_jump(body_jump);
	}

	self.statement();
	self.emit_loop(loop_start);

	if let Some(index) = exit_jump {
	    self.patch_jump(index);
	    self.emit_byte(Opcode::Pop as u8);
	}
	
	self.end_scope();
    }

    /// Compile an if statement
    fn if_statement(&mut self) {
	self.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
	self.expression();
	self.consume(TokenType::RightParen, "Expect ')' after condition.");

	let then_jump = self.emit_jump(Opcode::JumpIfFalse as u8);
	
	self.emit_byte(Opcode::Pop as u8);
	self.statement();

	let else_jump = self.emit_jump(Opcode::Jump as u8);
	
	self.patch_jump(then_jump);
	self.emit_byte(Opcode::Pop as u8);

	if self.match_token(TokenType::Else) {
	    self.statement();
	}

	self.patch_jump(else_jump);
    }

    /// Compile a print statement
    fn print_statement(&mut self) {
	self.expression();
	self.consume(TokenType::Semicolon, "Expect ';' after value.");
	self.emit_byte(Opcode::Print as u8);
    }

    /// Compile a while statement
    fn while_statement(&mut self) {
	let loop_start = self.current_chunk().count();
	
	self.consume(TokenType::LeftParen, "Expect '(' after 'while'.");
	self.expression();
	self.consume(TokenType::RightParen, "Expect ')' after condition.");

	let exit_jump = self.emit_jump(Opcode::JumpIfFalse as u8);

	self.emit_byte(Opcode::Pop as u8);
	self.statement();
	self.emit_loop(loop_start);

	self.patch_jump(exit_jump);
	self.emit_byte(Opcode::Pop as u8);
    }

    /// Advance to the possible beginning/end of a statement
    fn synchronize(&mut self) {
	self.parser.panic_mode = false;

	while self.parser.current.token_type() != TokenType::EOF {
	    if self.parser.previous.token_type() == TokenType::Semicolon {
		return;
	    }

	    match self.parser.current.token_type() {
		TokenType::Class |
		TokenType::Function |
		TokenType::Var |
		TokenType::For |
		TokenType::If |
		TokenType::While |
		TokenType::Print |
		TokenType::Return => return,

		_ => {}
	    }

	    self.advance();
	}
    }

    /// Compile a declaration
    fn declaration(&mut self) {
	if self.match_token(TokenType::Var) {
	    self.var_declaration();
	}
	else {
	    self.statement();
	}

	if self.parser.panic_mode {
	    self.synchronize();
	}
    }

    /// Compile a statement
    fn statement(&mut self) {
	if self.match_token(TokenType::Print) {
	    self.print_statement();
	}
	else if self.match_token(TokenType::For) {
	    self.for_statement();
	}
	else if self.match_token(TokenType::If) {
	    self.if_statement();
	}
	else if self.match_token(TokenType::While) {
	    self.while_statement();
	}
	else if self.match_token(TokenType::LeftBrace) {
	    self.begin_scope();
	    self.block();
	    self.end_scope();
	}
	else {
	    self.expression_statement();
	}
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
    fn error_at(&mut self, token: Token<'a>, message: &str) {
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

pub(crate) fn compile(source: &str, chunk: &mut Chunk, heap: &mut Heap, globals: &mut Globals) -> Result<(), CompileError> {
    let mut compiler = Compiler::new(source, chunk, heap, globals);

    compiler.advance();

    while !compiler.match_token(TokenType::EOF) {
	compiler.declaration();
    }

    compiler.end();

    if let Some(error) = compiler.parser.error.take() {
	Err(error)
    }
    else {
	Ok(())
    }
}
