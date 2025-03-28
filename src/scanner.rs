use std::string::ToString;

use strum_macros::Display;

#[derive(Debug, Display, PartialEq, Eq, Clone, Copy, Hash)]
pub(crate) enum TokenType {
    // Single char tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Percent,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two char tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    For,
    Function,
    If,
    Null,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    // Other
    Error,
    EOF
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Token<'a> {
    token_type: TokenType,
    lexeme: &'a str,
    line: usize
}

impl <'a> PartialEq for Token<'a> {
    fn eq(&self, other: &Self) -> bool {
	self.token_type == other.token_type && self.lexeme == other.lexeme
    }
}

impl <'a> Token<'a> {
    pub(crate) fn dummy() -> Self {
	Self {
	    token_type: TokenType::EOF,
	    lexeme: "",
	    line: 0
	}
    }
    
    pub(crate) fn token_type(&self) -> TokenType {
	self.token_type
    }

    pub(crate) fn lexeme(&self) -> &'a str {
	self.lexeme
    }

    pub(crate) fn line(&self) -> usize {
	self.line
    }
}

pub(crate) struct Scanner<'a> {
    source: &'a str,
    chars: Vec<char>,
    start: usize,
    current: usize,
    line: usize
}

impl <'a> Scanner<'a> {
    pub(crate) fn new(source: &'a str) -> Self {
	Self {
	    source: source,
	    chars: source.chars().collect(),
	    start: 0,
	    current: 0,
	    line: 1
	}
    }

    /// Scan through the source string and create the next Token
    pub(crate) fn scan_token(&mut self) -> Token<'a> {
	self.skip_whitespace();
	
	self.start = self.current;

	if self.is_at_end() {
	    return self.make_token(TokenType::EOF);
	}

	let c = self.advance();

	match c {
	    // Single char tokens
	    '(' => self.make_token(TokenType::LeftParen),
	    ')' => self.make_token(TokenType::RightParen),
	    '{' => self.make_token(TokenType::LeftBrace),
	    '}' => self.make_token(TokenType::RightBrace),
	    ';' => self.make_token(TokenType::Semicolon),
	    ',' => self.make_token(TokenType::Comma),
	    '.' => self.make_token(TokenType::Dot),
	    '-' => self.make_token(TokenType::Minus),
	    '+' => self.make_token(TokenType::Plus),
	    '/' => self.make_token(TokenType::Slash),
	    '*' => self.make_token(TokenType::Star),
	    '%' => self.make_token(TokenType::Percent),

	    // One or two char tokens
	    '!' => {
		let token_type = if self.match_next('=') {TokenType::BangEqual} else {TokenType::Bang};
		self.make_token(token_type)
	    },
	    '=' => {
		let token_type = if self.match_next('=') {TokenType::EqualEqual} else {TokenType::Equal};
		self.make_token(token_type)
	    },
	    '<' => {
		let token_type = if self.match_next('=') {TokenType::LessEqual} else {TokenType::Less};
		self.make_token(token_type)
	    },
	    '>' => {
		let token_type = if self.match_next('=') {TokenType::GreaterEqual} else {TokenType::Greater};
		self.make_token(token_type)
	    },

	    // Literal tokens
	    '"' => self.string(),
	    c if c.is_ascii_alphabetic() => self.identifier(),
	    c if c.is_ascii_digit() => self.number(),

	    _ => self.error_token("Unexpected character.")
	}
    }

    /// Run the scanner past any whitespace
    fn skip_whitespace(&mut self) {
	loop {
	    let c = self.peek();

	    match c {
		' ' | '\r' | '\t' => { self.advance(); },
		'\n' => {
		    self.line += 1;
		    self.advance();
		},
		'/' => if self.peek_next() == '/' {
		    while self.peek() != '\n' && !self.is_at_end() {
			self.advance();
		    }
		} else {
		    return;
		},
		_ => return
	    }
	}
    }

    /// Determine if the token being scanned is a keyword
    fn check_keyword(&self, start: usize, length: usize, rest: &'static str, token_type: TokenType) -> TokenType {
	if self.current - self.start == start + length && &self.source[self.start + start..self.current] == rest {
	    return token_type;
	}

	TokenType::Identifier
    }

    /// Determine the type of identifier currently being scanned
    fn identifier_type(&self) -> TokenType {
	match self.chars[self.start] {
	    'a' => self.check_keyword(1, 2, "nd", TokenType::And),
	    'c' => self.check_keyword(1, 4, "lass", TokenType::Class),
	    'e' => self.check_keyword(1, 3, "lse", TokenType::Else),
	    'i' => self.check_keyword(1, 1, "f", TokenType::If),
	    'n' => self.check_keyword(1, 3, "ull", TokenType::Null),
	    'o' => self.check_keyword(1, 1, "r", TokenType::Or),
	    'p' => self.check_keyword(1, 4, "rint", TokenType::Print),
	    'r' => self.check_keyword(1, 5, "eturn", TokenType::Return),
	    's' => self.check_keyword(1, 4, "uper", TokenType::Super),
	    'v' => self.check_keyword(1, 2, "ar", TokenType::Var),
	    'w' => self.check_keyword(1, 4, "hile", TokenType::While),

	    'f' => if self.current - self.start > 1 {
		match self.chars[self.start + 1] {
		    'a' => self.check_keyword(2, 3, "lse", TokenType::False),
		    'o' => self.check_keyword(2, 1, "r", TokenType::For),
		    'u' => self.check_keyword(2, 6, "nction", TokenType::Function),
		    _ => TokenType::Identifier
		}
	    } else {
		TokenType::Identifier
	    },

	    't' => if self.current - self.start > 1 {
		match self.chars[self.start + 1] {
		    'h' => self.check_keyword(2, 2, "is", TokenType::This),
		    'r' => self.check_keyword(2, 2, "ue", TokenType::True),
		    _ => TokenType::Identifier
		}
	    } else {
		TokenType::Identifier
	    }

	    _ => TokenType::Identifier
	}
    }

    /// Create an identifier token based on the scanner's start and current positions
    fn identifier(&mut self) -> Token<'a> {
	while self.peek().is_ascii_alphabetic() || self.peek().is_ascii_digit() {
	    self.advance();
	}

	self.make_token(self.identifier_type())
    }

    /// Create a number token based on the scanner's start and current positions
    fn number(&mut self) -> Token<'a> {
	while self.peek().is_ascii_digit() {
	    self.advance();
	}

	// Consume a decimal point if there is one
	if self.peek() == '.' && self.peek_next().is_ascii_digit() {
	    self.advance();
	}

	while self.peek().is_ascii_digit() {
	    self.advance();
	}

	self.make_token(TokenType::Number)
    }

    /// Create a string token based on the scanner's start and current positions
    fn string(&mut self) -> Token<'a> {
	while self.peek() != '"' && !self.is_at_end() {
	    if self.peek() == '\n' {
		self.line += 1;
	    }

	    self.advance();
	}

	if self.is_at_end() {
	    return self.error_token("Unterminated string.");
	}

	self.advance(); // Consume the closing quote

	self.make_token(TokenType::String)
    }

    /// Is the scanner at the end of the source string?
    fn is_at_end(&self) -> bool {
	self.current == self.chars.len()
    }

    /// Move the scanner forward and return the previous character
    fn advance(&mut self) -> char {
	self.current += 1;
	self.chars[self.current - 1]
    }

    /// Return the char at the scanner's current location
    fn peek(&self) -> char {
	if self.is_at_end() {
	    return '\0'
	}
	
	self.chars[self.current]
    }

    /// Return the char after the scanner's current position
    fn peek_next(&self) -> char {
	if self.is_at_end() || self.current == self.chars.len() - 1 {
	    return '\0';
	}

	self.chars[self.current + 1]
    }

    /// Check if the next char matches the expected char
    fn match_next(&mut self, expected: char) -> bool {
	if self.is_at_end() {
	    return false;
	}
	if self.chars[self.current] != expected {
	    return false;
	}

	self.current += 1;

	true
    }

    /// Create a token based on the scanner's start and current positions
    fn make_token(&self, token_type: TokenType) -> Token<'a> {
	Token {
	    token_type,
	    lexeme: &self.source[self.start..self.current],
	    line: self.line
	}
    }

    /// Create an error token with the given message
    fn error_token(&self, message: &'static str) -> Token<'a> {
	Token {
	    token_type: TokenType::Error,
	    lexeme: message,
	    line: self.line
	}
    }
}
