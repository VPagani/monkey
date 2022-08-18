use crate::token::{Token, TokenType};

pub struct Lexer {
	pub input: String,
	pub position: usize,
	pub read_position: usize,
	pub current_char: char,
}

fn is_digit(char: char) -> bool {
	return '0' <= char && char <= '9';
}

fn is_letter(char: char) -> bool {
	return 'a' <= char && char <= 'z' || 'A' <= char && char <= 'Z' || char == '_';
}

fn is_whitespace(char: char) -> bool {
	return match char {
		' ' => true,
		'\n' => true,
		'\t' => true,
		'\r' => true,
		_ => false,
	}
}

impl Lexer {
	pub fn new<'a>(input: String) -> Lexer {
		let mut lexer = Lexer { input, position: 0, read_position: 0, current_char: '\0' };
		lexer.read_char();
		return lexer;
	}

	fn peek_char(self: &mut Self) -> char {
		if self.read_position >= self.input.len() {
			return '\0';
		} else {
			return self.input.as_bytes()[self.read_position] as char;
		}
	}

	fn read_char(self: &mut Self) {
		if self.read_position >= self.input.len() {
			self.current_char = '\0';
		} else {
			self.current_char = self.input.as_bytes()[self.read_position] as char;
		}

		self.position = self.read_position;
		self.read_position += 1;
	}

	pub fn next_token(self: &mut Self) -> Token {
		use TokenType::*;

		let token: Token;

		self.skip_whitespace();

		match self.current_char {
			// Operators
			'=' => if self.peek_char() == '=' {
				let ch = self.current_char;
				self.read_char();

				let literal = format!("{}{}", ch, self.current_char);
				token = Token { ttype: Equal, literal };
			} else {
				token = Token::char(Assign, self.current_char);
			}

			'+' => token = Token::char(Plus, self.current_char),
			'-' => token = Token::char(Minus, self.current_char),
			'!' => if self.peek_char() == '=' {
				let ch = self.current_char;
				self.read_char();
				let literal = format!("{}{}", ch, self.current_char);
				token = Token { ttype: NotEqual, literal };
			} else {
				token = Token::char(Bang, self.current_char);
			}
			'*' => token = Token::char(Asterisk, self.current_char),
			'/' => token = Token::char(Slash, self.current_char),
			'<' => token = Token::char(LowerThan, self.current_char),
			'>' => token = Token::char(GreaterThan, self.current_char),

			// Delimiters
			',' => token = Token::char(Comma, self.current_char),
			';' => token = Token::char(Semicolon, self.current_char),
			'(' => token = Token::char(LParen, self.current_char),
			')' => token = Token::char(RParen, self.current_char),
			'{' => token = Token::char(LBrace, self.current_char),
			'}' => token = Token::char(RBrace, self.current_char),

			'\0' => {
				token = Token {
					ttype: EOF,
					literal: "".to_string(),
				}
			},

			_ => if is_letter(self.current_char) {
				let literal =  self.read_identifier();
				token = Token::from_identifier(literal);
				return token;
			} else if is_digit(self.current_char) {
				token = Token {
					ttype: Int,
					literal: self.read_number().to_string(),
				};
				return token;
			} else {
				token = Token::char(Illegal, self.current_char);
			},
		}

		self.read_char();

		return token;
	}

	fn read_number(self: &mut Self) -> &str {
		let position = self.position;

		while is_digit(self.current_char) {
			self.read_char();
		}

		return &self.input[position..self.position];

	}

	fn read_identifier(self: &mut Self) -> &str {
		let position = self.position;

		while is_letter(self.current_char) {
			self.read_char();
		}

		return &self.input[position..self.position];
	}

	fn skip_whitespace(self: &mut Self) {
		while is_whitespace(self.current_char) {
			self.read_char();
		}
	}
}


#[cfg(test)]
mod tests {
	use crate::token::{TokenType, TokenType::*};
	use super::Lexer;



	#[test]
	fn next_token() {
		let input = "
		let five = 5;
		let ten = 10;
	
		let add = fn(x, y) {
			x + y;
		};
	
		let result = add(five, ten);
		!-/*5;
		5 < 10 > 5;
	
		if (5 < 10) {
			return true;
		} else {
			return false;
		}
	
		10 == 10;
		10 != 9;
		";

		let tests: Vec<(TokenType, &str)> = vec![
			(Let, "let"),
			(Ident, "five"),
			(Assign, "="),
			(Int, "5"),
			(Semicolon, ";"),
	
			(Let, "let"),
			(Ident, "ten"),
			(Assign, "="),
			(Int, "10"),
			(Semicolon, ";"),
		
			(Let, "let"),
			(Ident, "add"),
			(Assign, "="),
			(Function, "fn"),
			(LParen, "("),
			(Ident, "x"),
			(Comma, ","),
			(Ident, "y"),
			(RParen, ")"),
			(LBrace, "{"),

			(Ident, "x"),
			(Plus, "+"),
			(Ident, "y"),
			(Semicolon, ";"),
			(RBrace, "}"),
			(Semicolon, ";"),
		
			(Let, "let"),
			(Ident, "result"),
			(Assign, "="),
			(Ident, "add"),
			(LParen, "("),
			(Ident, "five"),
			(Comma, ","),
			(Ident, "ten"),
			(RParen, ")"),
			(Semicolon, ";"),
		
			(Bang, "!"),
			(Minus, "-"),
			(Slash, "/"),
			(Asterisk, "*"),
			(Int, "5"),
			(Semicolon, ";"),
	
			(Int, "5"),
			(LowerThan, "<"),
			(Int, "10"),
			(GreaterThan, ">"),
			(Int, "5"),
			(Semicolon, ";"),
	
			(If, "if"),
			(LParen, "("),
			(Int, "5"),
			(LowerThan, "<"),
			(Int, "10"),
			(RParen, ")"),
			(LBrace, "{"),
	
			(Return, "return"),
			(True, "true"),
			(Semicolon, ";"),
	
			(RBrace, "}"),
			(Else, "else"),
			(LBrace, "{"),
	
			(Return, "return"),
			(False, "false"),
			(Semicolon, ";"),
	
			(RBrace, "}"),
	
			(Int, "10"),
			(Equal, "=="),
			(Int, "10"),
			(Semicolon, ";"),
	
			(Int, "10"),
			(NotEqual, "!="),
			(Int, "9"),
			(Semicolon, ";"),
	
			(EOF, ""),
		];

		let mut lexer = Lexer::new(input.to_string());

		for (i, expected_token) in tests.iter().enumerate() {
			let token = lexer.next_token();
			
			println!("tests[{}] = token type={:?}, literal={:?}", i, token.ttype, token.literal);

			assert_eq!(token.ttype, expected_token.0);
			// if token.ttype != expected_token.ttype {
			// 	eprintln!("tests[{}] = token type wrong, expected={:?}, got={:?}", i, token.ttype, expected_token.ttype);
			// }
			
			assert_eq!(token.literal, expected_token.1);
			// if token.literal != expected_token.literal {
			// 	eprintln!("tests[{}] = token literal wrong, expected={:?}, got={:?}", i, token.literal, expected_token.literal);
			// }
		}
	}
}