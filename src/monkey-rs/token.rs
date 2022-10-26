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
	GreaterThan,


	// Delimiters
	Comma,
	Semicolon,

	LParen,
	RParen,
	LBrace,
	RBrace,

	// Keywords
	Function,
	Let,
	True,
	False,
	If,
	Else,
	Return,
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

		_ => TokenType::Ident,
	}
}