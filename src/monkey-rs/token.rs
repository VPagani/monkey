#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
	Illegal,
	EOF,

	// Identifiers + Literals
	Ident,
	Int,

	// Operators
	Equal,
	NotEqual,
	Assign,
	Plus,
	Minus,
	Bang,
	Asterisk,
	Slash,

	LowerThan,
	LowerThanOrEqual,
	GreaterThan,
	GreaterThanOrEqual,


	// Delimiters
	Comma,
	Colon,
	Semicolon,

	LParen,
	RParen,
	LBrace,
	RBrace,
	LBracket,
	RBracket,

	// Keywords
	Macro,
	Function,
	Let,
	True,
	False,
	If,
	Else,
	Return,

	String,
}

impl TokenType {
	pub fn inspect<'a>(self) -> &'a str {
		match self {
			TokenType::Illegal => "ILLEGAL",
			TokenType::EOF => "EOF",

			TokenType::Ident => "IDENTIFIER",
			TokenType::Int => "INTEGER",

			TokenType::Bang => "!",
			TokenType::Equal => "==",
			TokenType::NotEqual => "!=",
			TokenType::Assign => "=",
			TokenType::Plus => "+",
			TokenType::Minus => "-",
			TokenType::Asterisk => "*",
			TokenType::Slash => "/",

			TokenType::LowerThan => "<",
			TokenType::LowerThanOrEqual => "<=",
			TokenType::GreaterThan => ">",
			TokenType::GreaterThanOrEqual => ">=",

			TokenType::Comma => ",",
			TokenType::Colon => ":",
			TokenType::Semicolon => ";",

			TokenType::LParen => "(",
			TokenType::RParen => ")",
			TokenType::LBrace => "{",
			TokenType::RBrace => "}",
			TokenType::LBracket => "[",
			TokenType::RBracket => "]",

			TokenType::Macro => "MACRO",
			TokenType::Function => "FUNCTION",
			TokenType::Let => "LET",
			TokenType::True => "TRUE",
			TokenType::False => "FALSE",
			TokenType::If => "IF",
			TokenType::Else => "ELSE",
			TokenType::Return => "RETURN",

			TokenType::String => "STRING",
		}
	}
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
	pub ttype: TokenType,
	pub literal: String,
}

impl Token {
	pub fn char(ttype: TokenType, char: char) -> Self {
		return Token {
			ttype,
			literal: char.to_string()
		};
	}

	pub fn str(ttype:TokenType, str: &str) -> Self {
		return Token { ttype, literal: str.to_string() };
	}

	pub fn string(ttype: TokenType, string:String) -> Self {
		return Token { ttype, literal: string };
	}

	pub fn from_identifier(identifier: &str) -> Self {
		let ttype = lookup_identifier(identifier);
		return Token { ttype, literal: identifier.to_string() };
	}
}

pub fn lookup_identifier(text: &str) -> TokenType {
	return match text {
		"fn" => TokenType::Function,
		"let" => TokenType::Let,
		"true" => TokenType::True,
		"false" => TokenType::False,
		"if" => TokenType::If,
		"else" => TokenType::Else,
		"return" => TokenType::Return,
		"macro" => TokenType::Macro,

		_ => TokenType::Ident,
	}
}