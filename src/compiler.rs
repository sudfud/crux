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
		(TokenType::LeftParen, ParseRule::new(Some(grouping), None, Precedence::None)),
		(TokenType::RightParen, ParseRule::new(None, None, Precedence::None)),
		(TokenType::LeftBrace, ParseRule::new(None, None, Precedence::None)),
		(TokenType::RightBrace, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Comma, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Dot, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Minus, ParseRule::new(Some(unary), Some(binary), Precedence::Term)),
		(TokenType::Plus, ParseRule::new(None, Some(binary), Precedence::Term)),
		(TokenType::Semicolon, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Slash, ParseRule::new(None, Some(binary), Precedence::Factor)),
		(TokenType::Star, ParseRule::new(None, Some(binary), Precedence::Factor)),
		(TokenType::Bang, ParseRule::new(Some(unary), None, Precedence::None)),
		(TokenType::BangEqual, ParseRule::new(None, Some(binary), Precedence::Equality)),
		(TokenType::Equal, ParseRule::new(None, None, Precedence::None)),
		(TokenType::EqualEqual, ParseRule::new(None, Some(binary), Precedence::Equality)),
		(TokenType::Greater, ParseRule::new(None, Some(binary), Precedence::Comparison)),
		(TokenType::GreaterEqual, ParseRule::new(None, Some(binary), Precedence::Comparison)),
		(TokenType::Less, ParseRule::new(None, Some(binary), Precedence::Comparison)),
		(TokenType::LessEqual, ParseRule::new(None, Some(binary), Precedence::Comparison)),
		(TokenType::Identifier, ParseRule::new(Some(variable), None, Precedence::None)),
		(TokenType::String, ParseRule::new(Some(string), None, Precedence::None)),
		(TokenType::Number, ParseRule::new(Some(number), None, Precedence::None)),
		(TokenType::And, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Class, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Else, ParseRule::new(None, None, Precedence::None)),
		(TokenType::False, ParseRule::new(Some(literal), None, Precedence::None)),
		(TokenType::For, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Function, ParseRule::new(None, None, Precedence::None)),
		(TokenType::If, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Null, ParseRule::new(Some(literal), None, Precedence::None)),
		(TokenType::Or, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Print, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Return, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Super, ParseRule::new(None, None, Precedence::None)),
		(TokenType::This, ParseRule::new(None, None, Precedence::None)),
		(TokenType::True, ParseRule::new(Some(literal), None, Precedence::None)),
		(TokenType::Var, ParseRule::new(None, None, Precedence::None)),
		(TokenType::While, ParseRule::new(None, None, Precedence::None)),
		(TokenType::Error, ParseRule::new(None, None, Precedence::None)),
		(TokenType::EOF, ParseRule::new(None, None, Precedence::None)),
	    ])
	}
    }
}

pub(crate) fn compile(source: &str, chunk: &mut Chunk, heap: &mut Heap, globals: &mut Globals) -> Result<(), CompileError> {
    let mut compiler = Compiler::new(source, chunk, heap, globals);

    advance(&mut compiler);

    while !match_token(&mut compiler, TokenType::EOF) {
	declaration(&mut compiler);
    }

    end(&mut compiler);

    if let Some(error) = compiler.parser.error.take() {
	Err(error)
    }
    else {
	Ok(())
    }
}

fn current_chunk<'a, 'b>(compiler: &'b Compiler<'a>) -> &'b Chunk
where 'a: 'b {
    compiler.chunk
}

fn current_chunk_mut<'a, 'b>(compiler: &'b mut Compiler<'a>) -> &'b mut Chunk
where 'a: 'b {
    compiler.chunk
}

/// Scan for the next token
fn advance<'a, 'b>(compiler: &'b mut Compiler<'a>)
where 'a: 'b {
    compiler.parser.previous = compiler.parser.current;

    loop {
	let token = compiler.scanner.scan_token();
	
	compiler.parser.current = token;

	if compiler.parser.current.token_type() != TokenType::Error {
	    break;
	}

	error_at_current(compiler, compiler.parser.current.lexeme());
    }
}

/// Check if the parser's current token matches the given type
fn consume(compiler: &mut Compiler, token_type: TokenType, message: &'static str) {
    if compiler.parser.current.token_type() == token_type {
	advance(compiler);
	return;
    }

    error_at_current(compiler, message);
}

/// Check if the current token matches the given type
fn check_token(compiler: &Compiler, token_type: TokenType) -> bool {
    compiler.parser.current.token_type() == token_type
}

/// Advance to the next token if the current one matches the given type
fn match_token(compiler: &mut Compiler, token_type: TokenType) -> bool {
    if !check_token(compiler, token_type) {
	return false;
    }

    advance(compiler);
    true
}

/// Write a single byte to the current chunk
fn emit_byte<'a, 'b>(compiler: &'b mut Compiler<'a>, byte: u8)
where 'a: 'b {
    let line = compiler.parser.previous.line();
    current_chunk_mut(compiler).push_byte(byte, line);
}

/// Write two bytes to the current chunk
fn emit_bytes<'a, 'b>(compiler: &'b mut Compiler<'a>, byte1: u8, byte2: u8)
where 'a: 'b {
    emit_byte(compiler, byte1);
    emit_byte(compiler, byte2);
}

/// Write a jump address to the current chunk
fn emit_jump(compiler: &mut Compiler, instruction: u8) -> usize {
    emit_byte(compiler, instruction);
    emit_bytes(compiler, 0xFF, 0xFF);

    current_chunk(compiler).count() - 2
}

/// Write return opcode to the current chunk
fn emit_return(compiler: &mut Compiler) {
    emit_byte(compiler, Opcode::Return as u8);
}

fn make_constant<'a, 'b>(compiler: &'b mut Compiler<'a>, value: Value) -> usize
where 'a: 'b {
    let constant_index = current_chunk_mut(compiler).add_constant(value);

    if constant_index <= 0xFFFF {
	constant_index
    }
    else {
	error(compiler, "Too many constants in one chunk.");
	0
    }
}

/// Write a constant to the chunk's constant table
fn emit_constant(compiler: &mut Compiler, value: Value) {
    let constant_index = make_constant(compiler, value);
    
    if constant_index <= 0xFF {
	emit_bytes(compiler, Opcode::Constant as u8, constant_index as u8);
    }
    else if constant_index <= 0xFFFF {
	emit_byte(compiler, Opcode::ConstantLong as u8);
	emit_bytes(compiler, (constant_index & 0x00FF) as u8, ((constant_index & 0xFF00) >> 8) as u8);
    }
}

fn patch_jump(compiler: &mut Compiler, offset: usize) {
    let jump = current_chunk(compiler).count() - offset - 2;

    if jump > 0xFFFF {
	error(compiler, "Too much code to jump over.");
    }

    current_chunk_mut(compiler).write_byte(offset, (jump & 0xFF) as u8);
    current_chunk_mut(compiler).write_byte(offset + 1, ((jump & 0xFF00) >> 8) as u8);
}

/// End compilation
fn end(compiler: &mut Compiler) {
    emit_return(compiler);

    #[cfg(feature = "print_code")]
    if compiler.parser.error.is_none() {
	let chunk = &*current_chunk(compiler);
	debug::disassemble_chunk(chunk, compiler.globals, "code");
    }
}

/// Enter a local scope
fn begin_scope(compiler: &mut Compiler) {
    compiler.scope_depth += 1;
}

/// Exit a local scope
fn end_scope(compiler: &mut Compiler) {
    compiler.scope_depth -= 1;

    while compiler.locals.len() > 0 && compiler.locals[compiler.locals.len() - 1].depth > compiler.scope_depth {
	emit_byte(compiler, Opcode::Pop as u8);
	compiler.locals.pop();
    }
}

/// Compile a binary expression
fn binary(compiler: &mut Compiler, can_assign: bool) {
    let op_type = compiler.parser.previous.token_type();
    let rule = &compiler.parse_rules[&op_type];
    let next_prec = Precedence::from_repr(rule.precedence as u8 + 1)
	.unwrap_or(Precedence::Primary);

    parse_precedence(compiler, next_prec);

    match op_type {
	TokenType::BangEqual => emit_byte(compiler, Opcode::NotEqual as u8),
	TokenType::EqualEqual => emit_byte(compiler, Opcode::Equal as u8),
	TokenType::Greater => emit_byte(compiler, Opcode::Greater as u8),
	TokenType::GreaterEqual => emit_byte(compiler, Opcode::GreaterEqual as u8),
	TokenType::Less => emit_byte(compiler, Opcode::Less as u8),
	TokenType::LessEqual => emit_byte(compiler, Opcode::LessEqual as u8),
	TokenType::Plus => emit_byte(compiler, Opcode::Add as u8),
	TokenType::Minus => emit_byte(compiler, Opcode::Subtract as u8),
	TokenType::Star => emit_byte(compiler, Opcode::Multiply as u8),
	TokenType::Slash => emit_byte(compiler, Opcode::Divide as u8),
	_ => error(compiler, "Invalid token for binary expression.")
    }
}

/// Compile a literal expression
fn literal(compiler: &mut Compiler, can_assign: bool) {
    match compiler.parser.previous.token_type() {
	TokenType::False => emit_byte(compiler, Opcode::False as u8),
	TokenType::Null => emit_byte(compiler, Opcode::Null as u8),
	TokenType::True => emit_byte(compiler, Opcode::True as u8),
	_ => error(compiler, "Invalid token for literal expression.")
    }
}

/// Compile a grouped expression
fn grouping(compiler: &mut Compiler, can_assign: bool) {
    expression(compiler);
    consume(compiler, TokenType::RightParen, "Expect ')' after expression");
}

/// Compile a number literal
fn number(compiler: &mut Compiler, can_assign: bool) {
    match compiler.parser.previous.lexeme().parse::<f64>() {
	Ok(value) => emit_constant(compiler, Value::Number(value)),
	Err(e) => error(compiler, &e.to_string())
    }
}

/// Compile a string literal
fn string(compiler: &mut Compiler, can_assign: bool) {
    let text = compiler.parser.previous.lexeme();
    let string = compiler.heap.allocate_string((&text[1..text.len() - 1]).into());
    emit_constant(compiler, Value::Object(string));
}

fn named_variable(compiler: &mut Compiler, name: String, can_assign: bool) {
    let (get_op, set_op, index): (Opcode, Opcode, usize) = {
	if let Some(arg) = resolve_local(compiler, &name) {
	    (
		if arg <= 0xFF {Opcode::GetLocal} else {Opcode::GetLocalLong},
		if arg <= 0xFF {Opcode::SetLocal} else {Opcode::SetLocalLong},
		arg
	    )
	}
	else {
	    let arg = identifier_constant(compiler, &name);
	    (
		if arg <= 0xFF {Opcode::GetGlobal} else {Opcode::GetGlobalLong},
		if arg <= 0xFF {Opcode::SetGlobal} else {Opcode::SetGlobalLong},
		arg
	    )
	}
    };

    if can_assign && match_token(compiler, TokenType::Equal) {
	expression(compiler);
	
	if set_op == Opcode::SetLocal || set_op == Opcode::SetGlobal {
	    emit_bytes(compiler, set_op as u8, index as u8);
	}
	else {
	    emit_byte(compiler, set_op as u8);
	    emit_bytes(compiler, (index & 0xFF) as u8, (index >> 8) as u8);
	}
    }
    else {
	if get_op == Opcode::GetLocal || get_op == Opcode::GetGlobal {
	    emit_bytes(compiler, get_op as u8, index as u8);
	}
	else {
	    emit_byte(compiler, get_op as u8);
	    emit_bytes(compiler, (index & 0xFF) as u8, (index >> 8) as u8);
	}
    }
}

/// Compile a variable
fn variable(compiler: &mut Compiler, can_assign: bool) {
    let name = String::from(compiler.parser.previous.lexeme());
    named_variable(compiler, name, can_assign);
}

/// Compile a unary expression
fn unary(compiler: &mut Compiler, can_assign: bool) {
    let op_type: TokenType = compiler.parser.previous.token_type();

    parse_precedence(compiler, Precedence::Unary);

    match op_type {
	TokenType::Bang => emit_byte(compiler, Opcode::Not as u8),
	TokenType::Minus => emit_byte(compiler, Opcode::Negate as u8),
	// _ => unreachable!("Invalid token for unary expression")
	_ => error(compiler, "Invalid token for unary expression")
    }
}

/// Parse any expression with the given precedence or higher
fn parse_precedence(compiler: &mut Compiler, precedence: Precedence) {
    advance(compiler);

    let prefix_rule = &compiler.parse_rules[&compiler.parser.previous.token_type()].prefix;
    let can_assign = precedence <= Precedence::Assignment;

    match prefix_rule {
	Some(prefix) => prefix(compiler, can_assign),
	None => error(compiler, "Expect expression.")
    }

    while precedence <= compiler.parse_rules[&compiler.parser.current.token_type()].precedence {
	advance(compiler);
	let infix_rule = &compiler.parse_rules[&compiler.parser.previous.token_type()].infix;
	
	if let Some(infix) = infix_rule {
	    infix(compiler, can_assign);
	}

	if can_assign && match_token(compiler, TokenType::Equal) {
	    error(compiler, "Invalid assignment target.");
	}
    }
}

/// Write a variable identifier constant to the current chunk
fn identifier_constant(compiler: &mut Compiler, name: &str) -> usize {
    match compiler.globals.index(name) {
	Some(idx) => idx,
	None => compiler.globals.insert(String::from(name), Value::Undefined)
    }
}

/// Find the index of a local variable, return None if variable is global
fn resolve_local(compiler: &mut Compiler, name: &str) -> Option<usize> {
    for i in (0..compiler.locals.len()).rev() {
	let local = &compiler.locals[i];

	if local.name.lexeme() == name {
	    if local.depth == 0 {
		error(compiler, "Can't read local variable in its own initializer.");
	    }
	    return Some(i);
	}
    }

    None
}

/// Add a local variable to a compiler's local array
fn add_local<'a>(compiler: &mut Compiler<'a>, name: Token<'a>) {
    let local = Local {
	name,
	depth: 0
    };

    compiler.locals.push(local);
}

/// Declare a local variable
fn declare_variable(compiler: &mut Compiler) {
    if compiler.scope_depth == 0 {
	return;
    }

    let name = compiler.parser.previous.clone();

    for i in (0..compiler.locals.len()).rev() {
	let local = &compiler.locals[i];

	if local.depth != 0 && local.depth < compiler.scope_depth {
	    break;
	}

	if &name == &local.name {
	    error(compiler, "Already a variable with this name in scope.");
	}
    }

    add_local(compiler, name);
}

fn parse_variable(compiler: &mut Compiler, error_message: &'static str) -> usize {
    consume(compiler, TokenType::Identifier, error_message);

    declare_variable(compiler);
    
    if compiler.scope_depth > 0 {
	return 0;
    }
    
    let name = compiler.parser.previous.lexeme();
    
    identifier_constant(compiler, name)
}

/// Initialize a declared local variable
fn mark_initialized(compiler: &mut Compiler) {
    let count = compiler.locals.len();
    compiler.locals[count - 1].depth = compiler.scope_depth;
}

fn define_variable(compiler: &mut Compiler, global_index: usize) {
    if compiler.scope_depth > 0 {
	mark_initialized(compiler);
	return;
    }
    
    if global_index <= 0xFF {
	emit_bytes(compiler, Opcode::DefineGlobal as u8, global_index as u8);
    }
    else {
	emit_byte(compiler, Opcode::DefineGlobalLong as u8);
	emit_bytes(compiler, (global_index & 0x00FF) as u8, (global_index >> 8) as u8);
    }
}

fn get_rule<'a>(compiler: &'a Compiler<'a>, token_type: TokenType) -> &'a ParseRule {
    &compiler.parse_rules[&token_type]
}

/// Compile an expression
fn expression(compiler: &mut Compiler) {
    parse_precedence(compiler, Precedence::Assignment);
}

/// Compile a block surrounded by curly braces
fn block(compiler: &mut Compiler) {
    while !check_token(compiler, TokenType::RightBrace) && !check_token(compiler, TokenType::EOF) {
	declaration(compiler);
    }

    consume(compiler, TokenType::RightBrace, "Expect '}' after block.");
}

/// Compile a variable declaration
fn var_declaration(compiler: &mut Compiler) {
    let global_index = parse_variable(compiler, "Expect variable name.");

    if match_token(compiler, TokenType::Equal) {
	expression(compiler);
    }
    else {
	emit_byte(compiler, Opcode::Null as u8);
    }

    consume(compiler, TokenType::Semicolon, "Expect ';' after variable declaration.");
    define_variable(compiler, global_index);
}

/// Compile an expression statement
fn expression_statement(compiler: &mut Compiler) {
    expression(compiler);
    consume(compiler, TokenType::Semicolon, "Expect ';' after expression.");
    emit_byte(compiler, Opcode::Pop as u8);
}

/// Compile an if statement
fn if_statement(compiler: &mut Compiler) {
    consume(compiler, TokenType::LeftParen, "Expect '(' after 'if'.");
    expression(compiler);
    consume(compiler, TokenType::RightParen, "Expect ')' after condition.");

    let then_jump = emit_jump(compiler, Opcode::JumpIfFalse as u8);
    emit_byte(compiler, Opcode::Pop as u8);
    statement(compiler);

    let else_jump = emit_jump(compiler, Opcode::Jump as u8);
    
    patch_jump(compiler, then_jump);
    emit_byte(compiler, Opcode::Pop as u8);

    if match_token(compiler, TokenType::Else) {
	statement(compiler);
    }

    patch_jump(compiler, else_jump);
}

/// Compile a print statement
fn print_statement(compiler: &mut Compiler) {
    expression(compiler);
    consume(compiler, TokenType::Semicolon, "Expect ';' after value.");
    emit_byte(compiler, Opcode::Print as u8);
}

/// Advance to the possible beginning/end of a statement
fn synchronize(compiler: &mut Compiler) {
    compiler.parser.panic_mode = false;

    while compiler.parser.current.token_type() != TokenType::EOF {
	if compiler.parser.previous.token_type() == TokenType::Semicolon {
	    return;
	}

	match compiler.parser.current.token_type() {
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

	advance(compiler);
    }
}

/// Compile a declaration
fn declaration(compiler: &mut Compiler) {
    if match_token(compiler, TokenType::Var) {
	var_declaration(compiler);
    }
    else {
	statement(compiler);
    }

    if compiler.parser.panic_mode {
	synchronize(compiler);
    }
}

/// Compile a statement
fn statement(compiler: &mut Compiler) {
    if match_token(compiler, TokenType::Print) {
	print_statement(compiler);
    }
    else if match_token(compiler, TokenType::If) {
	if_statement(compiler);
    }
    else if match_token(compiler, TokenType::LeftBrace) {
	begin_scope(compiler);
	block(compiler);
	end_scope(compiler);
    }
    else {
	expression_statement(compiler);
    }
}

/// Report an error found at the previous token
fn error<'a, 'b>(compiler: &'b mut Compiler<'a>, message: &str)
where 'a: 'b {
    error_at(compiler, compiler.parser.previous, message);
}

/// Report an error found at the current token
fn error_at_current<'a, 'b>(compiler: &'b mut Compiler<'a>, message: &str)
where 'a: 'b {
    error_at(compiler, compiler.parser.current, message);
}

/// Report an error found at the given token
fn error_at<'a, 'b>(compiler: &'b mut Compiler<'a>, token: Token<'a>, message: &str)
where 'a: 'b {
    if compiler.parser.panic_mode {
	return;
    }

    compiler.parser.panic_mode = true;

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
    
    compiler.parser.error = Some(CompileError::new(message, &token));
}
