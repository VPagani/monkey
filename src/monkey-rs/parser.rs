use crate::{lexer::Lexer, token::{Token, TokenType}, ast::{Program, Statement}};

pub struct Parser<'a> {
	lexer: &'a mut Lexer<'a>,

	current_token: Token,
	peek_token: Token,

	pub errors: Vec<String>,
}

impl<'a> Parser<'a> {
	pub fn new(lexer: &'a mut Lexer<'a>) -> Parser<'a> {
		let current_token = lexer.next_token();
		let peek_token = lexer.next_token();

		Parser {
			lexer,

			current_token: current_token,
			peek_token: peek_token,

			errors: Vec::new(),
		}
	}

	fn next_token(&mut self) {
		let peek_token = self.lexer.next_token();
		self.current_token = self.peek_token.clone();
		self.peek_token = peek_token;
	}

	fn current_token_is(&mut self, t: TokenType) -> bool {
		self.current_token.ttype == t
	}

	fn peek_token_is(&mut self, t: TokenType) -> bool {
		self.peek_token.ttype == t
	}

	fn expect_peek(&mut self, t: TokenType) -> bool {
		if self.peek_token_is(t) {
			self.next_token();
			return true;
		}

		self.peek_error(t);
		return false;
	}

	fn peek_error(&mut self, ttype: TokenType) {
		let msg = format!("expected next token to be {:?}, got {:?} instead", ttype, self.peek_token.ttype);
		self.errors.push(msg);
	}

	fn skip_until_semicolons(&mut self) {
		while !self.current_token_is(TokenType::Semicolon) {
			self.next_token();
		}
	}
}

impl<'a> Parser<'a> {

	pub fn parse_program(&mut self) -> Program {
		let mut program = Program {
			statements: Vec::new(),
		};

		while self.current_token.ttype != TokenType::EOF {
			if let Some(statement) = self.parse_statement() {
				program.statements.push(statement);
			}
			self.next_token();
		}

		return program;
	}

	fn parse_statement(&mut self) -> Option<Statement> {
		match self.current_token.ttype {
			TokenType::Let => self.parse_let_statement(),
			TokenType::Return => self.parse_return_statement(),
			_ => None,
		}
	}

	fn parse_let_statement(&mut self) -> Option<Statement> {
		let token = self.current_token.clone();

		if !self.expect_peek(TokenType::Ident) {
			return None;
		}

		let name = self.current_token.literal.clone();

		if !self.expect_peek(TokenType::Assign) {
			return None;
		}

		// TODO: We're skipping the expressions until we encounter a semicolon
		self.skip_until_semicolons();

		return Some(Statement::Let{ token, name, value: None });
	}

	fn parse_return_statement(&mut self) -> Option<Statement> {
		let token = self.current_token.clone();

		self.next_token();

		// TODO: We're skipping the expressions until we encounter a semicolon
		self.skip_until_semicolons();

		return Some(Statement::Return { token, value: None });
	}
}

#[cfg(test)]
mod tests {
	use crate::{lexer::Lexer, parser::Parser, ast::Statement};

	fn check_parser_errors(parser: &Parser) {
		let errors_length = parser.errors.len();
		if errors_length == 0 {
			return;
		}

		eprintln!("parser has {} errors", errors_length);

		for msg in parser.errors.iter() {
			eprintln!("parser error: {}", msg);
		}

		panic!();
	}

	#[test]
	fn test_let_statements() {
		let input = "
		let x = 5;
		let y = 10;
		let foobar = 838383;
		";

		let mut lexer = Lexer::new(input);
		let mut parser = Parser::new(&mut lexer);

		let program = parser.parse_program();
		check_parser_errors(&parser);
		
		let statement_length = program.statements.len();
		if statement_length != 3 {
			eprintln!("program.statements does not contain 3 statements. got={}", statement_length);
		}

		let tests: Vec<&str> = vec![
			("x"),
			("y"),
			("foobar")
		];

		for (test, stmt) in tests.into_iter().zip(program.statements.into_iter()) {
			test_let_statement(test, stmt);
		}

		
		fn test_let_statement(let_name: &str, s: Statement) {
			assert!(matches!(s, Statement::Let { name, .. } if name == let_name));
		}
	}

	#[test]
	fn test_return_statements() {
		let input = "
		return 5;
		return 10;
		return 95345;
		";

		let mut lexer = Lexer::new(input);
		let mut parser = Parser::new(&mut lexer);

		let program = parser.parse_program();
		check_parser_errors(&parser);
		
		let statement_length = program.statements.len();
		if statement_length != 3 {
			eprintln!("program.statements does not contain 3 statements. got={}", statement_length);
		}

		for stmt in program.statements.into_iter() {
			assert!(matches!(stmt, Statement::Return { .. }));
		}
	}
}