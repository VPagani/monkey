use crate::{lexer::Lexer, token::{Token, TokenType}, ast};

pub struct Parser<'a> {
	lexer: &'a mut Lexer<'a>,

	current_token: Token,
	peek_token: Token,

	pub errors: Vec<String>,
}

#[derive(PartialEq, PartialOrd)]
enum Precedence {
	Lowest,
	Equality,
	Comparison,
	Term,
	Factor,
	Prefix,
	Call,
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

		self.add_peek_token_error(t);
		return false;
	}

	fn add_peek_token_error(&mut self, ttype: TokenType) {
		let msg = format!("expected next token to be {:?}, got {:?} instead", ttype, self.peek_token.ttype);
		self.errors.push(msg);
	}

	fn add_no_prefix_parse_error(&mut self, ttype: TokenType) {
		let msg = format!("no prefix parse function for {:?} found", ttype);
		self.errors.push(msg);
	}

	fn skip_until_semicolons(&mut self) {
		while !self.current_token_is(TokenType::Semicolon) {
			self.next_token();
		}
	}

	fn get_infix_precedence(&self, ttype: TokenType) -> Precedence {
		match ttype {
			TokenType::Equal | TokenType::NotEqual => Precedence::Equality,
			TokenType::GreaterThan | TokenType::LowerThan => Precedence::Comparison,
			TokenType::Plus | TokenType::Minus => Precedence::Term,
			TokenType::Asterisk | TokenType::Slash => Precedence::Factor,
			_ => Precedence::Lowest
		}
	}
}

impl<'a> Parser<'a> {
	pub fn parse_program(&mut self) -> ast::Program {
		let mut program = ast::Program {
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

	fn parse_statement(&mut self) -> Option<ast::Statement> {
		match self.current_token.ttype {
			TokenType::Let => self.parse_let_statement().map(ast::Statement::Let),
			TokenType::Return => self.parse_return_statement().map(ast::Statement::Return),
			_ => self.parse_expression_statement().map(ast::Statement::Expression),
		}
	}

	fn parse_let_statement(&mut self) -> Option<ast::LetStatement> {
		let token = self.current_token.clone();

		if !self.expect_peek(TokenType::Ident) {
			return None;
		}

		let name_token = self.current_token.clone();
		let name = ast::IdentifierExpression {
			value: name_token.literal.clone(),
			token: name_token,
		};

		if !self.expect_peek(TokenType::Assign) {
			return None;
		}

		// TODO: We're skipping the expressions until we encounter a semicolon
		self.skip_until_semicolons();

		return Some(
			ast::LetStatement {
				token,
				name,
				value: None
			}
		);
	}

	fn parse_return_statement(&mut self) -> Option<ast::ReturnStatement> {
		let token = self.current_token.clone();

		self.next_token();

		// TODO: We're skipping the expressions until we encounter a semicolon
		self.skip_until_semicolons();

		return Some(
			ast::ReturnStatement {
				token,
				value: None
			}
		);
	}

	fn parse_expression_statement(&mut self) -> Option<ast::ExpressionStatement> {
		let token = self.current_token.clone();

		let expression = self.parse_expression(Precedence::Lowest)?;

		if self.peek_token_is(TokenType::Semicolon) {
			self.next_token();
		}

		return Some(ast::ExpressionStatement { token, expression });
	}

	fn parse_expression(&mut self, precedence: Precedence) -> Option<ast::Expression> {
		let mut left_exp:ast::Expression;
		
		match self.parse_expression_prefix() {
			Some(expression) => left_exp = expression,
			None => {
				self.add_no_prefix_parse_error(self.current_token.ttype);
				return None;
			}
		};


		while !self.peek_token_is(TokenType::Semicolon) && precedence < self.get_infix_precedence(self.peek_token.ttype) {
			match self.parse_expression_infix(left_exp) {
				Ok(expression) => left_exp = expression,
				Err(expression) => {
					self.errors.push(format!("no infix parse function for {:?} found", self.current_token.ttype));
					return Some(expression);
				}
			}
		}

		return Some(left_exp);
	}

	fn parse_expression_prefix(&mut self) -> Option<ast::Expression> {
		let operator = self.current_token.clone();

		match operator.ttype {
			TokenType::Ident => Some(ast::Expression::Identifier(
				ast::IdentifierExpression {
					value: operator.literal.clone(),
					token: operator,
				}
			)),

			TokenType::Int => {
				let value = match operator.literal.parse::<i64>() {
					Ok(value) => value,
					Err(err) => {
						let msg = format!("could not parse '{}' as integer: {}", operator.literal, err);
						self.errors.push(msg);
						return None;
					}
				};

				Some(ast::Expression::IntegerLiteral(
					ast::IntegerLiteralExpression {
						value: value,
						token: operator,
					}
				))
			},

			TokenType::True | TokenType::False => {
				Some(ast::Expression::BooleanLiteral(ast::BooleanLiteralExpression {
					value: operator.ttype == TokenType::True,
					token: operator,
				}))
			}

			TokenType::Bang | TokenType::Minus => {
				self.next_token();

				let right = self.parse_expression(Precedence::Prefix)?;

				Some(ast::Expression::Prefix(ast::PrefixExpression {
					operator,
					right: Box::new(right),
				}))
			}

			TokenType::LParen => {
				self.next_token();

				let expression = self.parse_expression(Precedence::Lowest);

				if !self.expect_peek(TokenType::RParen) {
					return None;
				}

				return expression;
			}

			_ => None,
		}
	}

	fn parse_expression_infix(&mut self, left: ast::Expression) -> Result<ast::Expression, ast::Expression> {
		self.next_token();

		let operator = self.current_token.clone();

		return match operator.ttype {
			TokenType::Equal |
			TokenType::NotEqual |
			TokenType::GreaterThan |
			TokenType::LowerThan |
			TokenType::Plus |
			TokenType::Minus |
			TokenType::Asterisk |
			TokenType::Slash => {
				let precedence = self.get_infix_precedence(operator.ttype);

				self.next_token();

				let right = match self.parse_expression(precedence) {
					Some(expression) => expression,
					None => return Err(left),
				};

				Ok(ast::Expression::Infix(ast::InfixExpression {
					operator,
					left: Box::new(left),
					right: Box::new(right),
				}))
			}

			_ => Err(left),
		}

	}
}

#[cfg(test)]
mod tests {
	use crate::{
		ast,
		lexer::Lexer,
		parser::Parser,
		ast::{Node, Program, Statement, Expression},
	};

	fn parse(input: &str) -> Program {
		let mut lexer = Lexer::new(input);
		let mut parser = Parser::new(&mut lexer);

		let program = parser.parse_program();
		check_parser_errors(&parser);

		return program;
	}

	fn parse_statements(input: &str, expected_statements: usize) -> Program {
		let program = parse(input);

		let statement_length = program.statements.len();
		if statement_length != expected_statements {
			panic!("program.statements does not contain {} statements. got={}", expected_statements, statement_length);
		}

		return program;
	}

	fn parse_expression(input: &str) -> Expression {
		let mut program = parse_statements(input, 1);
		let statement = program.statements.pop();

		match statement {
			Some(Statement::Expression(statement)) => statement.expression,
			_ => {
				panic!("program.statement[0] is not Statement::Expression. got={:?}", statement);
			}
		}
	}

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

	fn check_let_statement(statement: Statement, name: &str) {
		let token_literal = statement.literal();
		if token_literal != "let" {
			panic!("statement.literal() not 'let'. got={}", token_literal);
		}

		if let Statement::Let(ast::LetStatement { name: ref name_identifier, .. }) = statement {
			if name_identifier.value != name {
				panic!("let_statement.name is not '{}'. got={}", name, name_identifier.value);
			}

			return;
		}

		panic!("statement is not LetStatement. got={:?}", statement);
	}

	#[test]
	fn test_let_statements() {
		let input = "
		let x = 5;
		let y = 10;
		let foobar = 838383;
		";

		let program = parse_statements(input, 3);

		let tests: Vec<&str> = vec![
			("x"),
			("y"),
			("foobar")
		];

		for (name, stmt) in tests.into_iter().zip(program.statements.into_iter()) {
			check_let_statement(stmt, name);
		}
	}

	#[test]
	fn test_return_statements() {
		let input = "
		return 5;
		return 10;
		return 95345;
		";

		let program = parse_statements(input, 3);

		for stmt in program.statements.into_iter() {
			assert!(matches!(stmt, Statement::Return { .. }));
		}
	}

	fn check_identifier(expression: Expression, value: &str) {
		match expression {
			Expression::Identifier(ast::IdentifierExpression { token, value: identifier_value, .. }) => {
				if identifier_value != value {
					panic!("identifier is not '{}'. got={}", value, identifier_value);
				}

				if token.literal != value {
					panic!("identifier.token.literal is not '{}'. got={}", value, token.literal);
				}
			}

			_ => {
				panic!("expression is not Identifier. got={:?}", expression);
			}
		}
	}

	fn check_integer_literal(expression: Expression, value: i64) {
		match expression {
			Expression::IntegerLiteral(ast::IntegerLiteralExpression { token, value: integer_value, .. }) => {
				if integer_value != value {
					panic!("integer is not '{}'. got={}", value, integer_value);
				}

				if token.literal != format!("{}", value) {
					panic!("integer.token.literal is not '{}'. got={}", value, token.literal);
				}
			},

			_ => {
				panic!("expression is not IntegerLiteral. got={:?}", expression);
			}
		}
	}

	fn check_boolean_literal(expression: Expression, value: bool) {
		match expression {
			Expression::BooleanLiteral(ast::BooleanLiteralExpression { token, value: boolean_value, .. }) => {
				if boolean_value != value {
					panic!("integer is not '{}'. got={}", value, boolean_value);
				}

				if token.literal != format!("{}", value) {
					panic!("integer.token.literal is not '{}'. got={}", value, token.literal);
				}
			},

			_ => {
				panic!("expression is not BooleanLiteral. got={:?}", expression);
			}
		}
	}

	enum LiteralValue<'a> {
		Integer(i64),
		Boolean(bool),
		Identifier(&'a str),
	}

	fn check_literal_expression(expression: Expression, expected_value: LiteralValue) {
		match expected_value {
			LiteralValue::Integer(value) => check_integer_literal(expression, value),
			LiteralValue::Boolean(value) => check_boolean_literal(expression, value),
			LiteralValue::Identifier(value) => check_identifier(expression, value),
		}
	}

	#[test]
	fn test_parsing_identifier_expression() {
		let input = "foobar;";

		let expression = parse_expression(input);

		check_identifier(expression, "foobar");
	}

	#[test]
	fn test_parsing_integer_literal_expression() {
		let input = "5;";

		let expression = parse_expression(input);

		check_integer_literal(expression, 5);

	}

	#[test]
	fn test_parsing_boolean_literal_expression() {
		let tests = vec![
			("true;", true),
			("false;", false),
		];

		for test in tests {
			let expression = parse_expression(test.0);

			check_boolean_literal(expression, test.1);
		}
	}

	#[test]
	fn test_parsing_prefix_expression() {
		let tests: Vec<(&str, &str, LiteralValue)> = vec![
			("!5;", "!", LiteralValue::Integer(5)),
			("-15;", "-", LiteralValue::Integer(15)),
		];

		for test in tests {
			let expression = parse_expression(test.0);

			match expression {
				Expression::Prefix(ast::PrefixExpression { operator, right }) => {
					assert_eq!(test.1, operator.literal);

					check_literal_expression(*right, test.2);
				},

				_ => {
					panic!("expression is not Prefix. got={:?}", expression);
				}
			}
		}
	}

	#[test]
	fn test_parsing_infix_expression() {
		let tests: Vec<(&str, LiteralValue, &str, LiteralValue)> = vec![
			("5 + 5;", LiteralValue::Integer(5), "+", LiteralValue::Integer(5)),
			("5 - 5;", LiteralValue::Integer(5), "-", LiteralValue::Integer(5)),
			("5 * 5;", LiteralValue::Integer(5), "*", LiteralValue::Integer(5)),
			("5 / 5;", LiteralValue::Integer(5), "/", LiteralValue::Integer(5)),
			("5 < 5;", LiteralValue::Integer(5), "<", LiteralValue::Integer(5)),
			("5 > 5;", LiteralValue::Integer(5), ">", LiteralValue::Integer(5)),
			("5 == 5;", LiteralValue::Integer(5), "==", LiteralValue::Integer(5)),
			("5 != 5;", LiteralValue::Integer(5), "!=", LiteralValue::Integer(5)),
			("true == true", LiteralValue::Boolean(true), "==", LiteralValue::Boolean(true)),
			("true != false", LiteralValue::Boolean(true), "!=", LiteralValue::Boolean(false)),
			("false == false", LiteralValue::Boolean(false), "==", LiteralValue::Boolean(false)),
		];

		for test in tests {
			let expression = parse_expression(test.0);

			match expression {
				Expression::Infix(ast::InfixExpression { operator, left, right }) => {
					assert_eq!(test.2, operator.literal);

					check_literal_expression(*left, test.1);
					check_literal_expression(*right, test.3);
				},

				_ => {
					panic!("expression is not Infix. got={:?}", expression);
				}
			}
		}
	}

	#[test]
	fn test_operator_precedence() {
		let tests: Vec<(&str, &str)> = vec![
			(
				"-a + b",
				"((-a) + b)",
			),
			(
				"!-a",
				"(!(-a))",
			),
			(
				"a + b + c",
				"((a + b) + c)",
			),
			(
				"a + b - c",
				"((a + b) - c)",
			),
			(
				"a * b * c",
				"((a * b) * c)",
			),
			(
				"a * b / c",
				"((a * b) / c)",
			),
			(
				"a + b / c",
				"(a + (b / c))",
			),
			(
				"a + b * c + d / e - f",
				"(((a + (b * c)) + (d / e)) - f)",
			),
			(
				"3 + 4; -5 * 5",
				"(3 + 4)((-5) * 5)",
			),
			(
				"5 > 4 == 3 < 4",
				"((5 > 4) == (3 < 4))",
			),
			(
				"5 < 4 != 3 > 4",
				"((5 < 4) != (3 > 4))",
			),
			(
				"3 + 4 * 5 == 3 * 1 + 4 * 5",
				"((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
			),
			(
				"true",
				"true",
			),
			(
				"false",
				"false",
			),
			(
				"3 > 5 == false",
				"((3 > 5) == false)",
			),
			(
				"3 < 5 == true",
				"((3 < 5) == true)",
			),
			(
				"1 + (2 + 3) + 4",
				"((1 + (2 + 3)) + 4)",
			),
			(
				"(5 + 5) * 2",
				"((5 + 5) * 2)",
			),
			(
				"2 / (5 + 5)",
				"(2 / (5 + 5))",
			),
			(
				"-(5 + 5)",
				"(-(5 + 5))",
			),
			(
				"!(true == true)",
				"(!(true == true))",
			),
		];

		for test in tests {
			let program = parse(test.0);

			assert_eq!(program.to_string(), test.1);
		}
	}
}