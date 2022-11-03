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
	Index,
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

	fn get_infix_precedence(&self, ttype: TokenType) -> Precedence {
		match ttype {
			TokenType::Equal | TokenType::NotEqual => Precedence::Equality,
			TokenType::GreaterThan | TokenType::LowerThan |
			TokenType::GreaterThanOrEqual | TokenType::LowerThanOrEqual => Precedence::Comparison,
			TokenType::Plus | TokenType::Minus => Precedence::Term,
			TokenType::Asterisk | TokenType::Slash => Precedence::Factor,
			TokenType::LParen => Precedence::Call,
			TokenType::LBracket => Precedence::Index,
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
			// TokenType::LBrace => self.parse_block_statement().map(ast::Statement::Block),
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

		self.next_token();

		let value = self.parse_expression(Precedence::Lowest);

		if self.peek_token_is(TokenType::Semicolon) {
			self.next_token()
		}

		return Some(
			ast::LetStatement {
				token,
				name,
				value,
			}
		);
	}

	fn parse_return_statement(&mut self) -> Option<ast::ReturnStatement> {
		let token = self.current_token.clone();

		self.next_token();

		let value = self.parse_expression(Precedence::Lowest);

		if !self.expect_peek(TokenType::Semicolon) {
			return None;
		}

		return Some(
			ast::ReturnStatement {
				token,
				value
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

	fn parse_block_statement(&mut self) -> Option<ast::BlockStatement> {
		let token = self.current_token.clone();
		let mut statements: Vec<ast::Statement> = vec![];

		self.next_token();

		while !self.current_token_is(TokenType::RBrace) && !self.current_token_is(TokenType::EOF) {
			statements.push(self.parse_statement()?);

			self.next_token();
		}

		return Some(ast::BlockStatement {
			token,
			statements,
		});
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

			TokenType::String => {
				Some(ast::Expression::StringLiteral(
					ast::StringLiteralExpression {
						value: operator.literal.clone(),
						token: operator
					}
				))
			}

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

			TokenType::LBracket => {
				self.next_token();
				
				let elements = self.parse_expression_list(TokenType::RBracket)?;

				return Some(ast::Expression::ArrayLiteral(
					ast::ArrayLiteralExpression {
						token: operator,
						elements,
					}
				))
			}

			TokenType::LBrace => {
				let mut pairs = vec![];

				while !self.peek_token_is(TokenType::RBrace) {
					self.next_token();
					let key = self.parse_expression(Precedence::Lowest)?;

					if !self.expect_peek(TokenType::Colon) {
						return None;
					}

					self.next_token();
					let value = self.parse_expression(Precedence::Lowest)?;

					pairs.push((key, value));

					if !self.peek_token_is(TokenType::RBrace) && !self.expect_peek(TokenType::Comma) {
						return None;
					}
				}

				if !self.expect_peek(TokenType::RBrace) {
					return None;
				}

				Some(ast::Expression::HashLiteral(ast::HashLiteralExpression {
					token: operator,
					pairs,
				}))
			}

			TokenType::If => {
				if !self.expect_peek(TokenType::LParen) {
					return None;
				}

				self.next_token();

				let condition = Box::new(self.parse_expression(Precedence::Lowest)?);

				if !self.expect_peek(TokenType::RParen) {
					return None;
				}

				if !self.expect_peek(TokenType::LBrace) {
					return None;
				}

				let consequence = self.parse_block_statement()?;
				let mut alternative = None;

				if self.peek_token_is(TokenType::Else) {
					self.next_token();

					if !self.expect_peek(TokenType::LBrace) {
						return None;
					}

					alternative = Some(self.parse_block_statement()?);
				}

				return Some(ast::Expression::If(
					ast::IfExpression {
						token: operator,
						condition,
						consequence,
						alternative
					}
				));
			}

			TokenType::Function => {
				if !self.expect_peek(TokenType::LParen) {
					return None;
				}

				let parameters = self.parse_function_parameters()?;

				if !self.expect_peek(TokenType::LBrace) {
					return None;
				}

				let body = self.parse_block_statement()?;

				return Some(ast::Expression::Function(ast::FunctionExpression {
					token: operator,
					parameters,
					body,
				}))
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
			TokenType::GreaterThanOrEqual |
			TokenType::LowerThanOrEqual |
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

			TokenType::LParen => {
				self.next_token();

				let arguments = match self.parse_expression_list(TokenType::RParen) {
					Some(arguments) => arguments,
					None => return Err(left),
				};

				Ok(ast::Expression::Call(ast::CallExpression {
					token: operator,
					identifier: Box::new(left),
					arguments,
				}))
			}

			TokenType::LBracket => {
				self.next_token();

				let index = match self.parse_expression(Precedence::Lowest) {
					Some(expression) => expression,
					None => return Err(left),
				};

				if !self.expect_peek(TokenType::RBracket) {
					return Err(left);
				}

				Ok(ast::Expression::Index(
					ast::IndexExpression {
						token: operator,
						left: Box::new(left),
						index: Box::new(index),
					}
				))
			}

			_ => Err(left),
		}
	}

	fn parse_expression_list(&mut self, end: TokenType) -> Option<Vec<ast::Expression>> {
		let mut args = vec![];

		if !self.current_token_is(end) {
			args.push(self.parse_expression(Precedence::Lowest)?);

			while self.peek_token_is(TokenType::Comma) {
				self.next_token();
				self.next_token();
				
				args.push(self.parse_expression(Precedence::Lowest)?);
			}

			if !self.expect_peek(end) {
				return None;
			}
		}

		return Some(args);
	}

	fn parse_function_parameters(&mut self) -> Option<Vec<ast::IdentifierExpression>> {
		let mut identifiers: Vec<ast::IdentifierExpression> = vec![];

		if self.peek_token_is(TokenType::RParen) {
			self.next_token();
			return Some(identifiers);
		}

		self.next_token();
		let token = self.current_token.clone();
		identifiers.push(
			ast::IdentifierExpression {
				value: token.literal.clone(),
				token,
			}
		);

		while self.peek_token_is(TokenType::Comma) {
			self.next_token();
			self.next_token();

			let token = self.current_token.clone();
			identifiers.push(
				ast::IdentifierExpression {
					value: token.literal.clone(),
					token,
				}
			);
		}

		if !self.expect_peek(TokenType::RParen) {
			return None;
		}

		return Some(identifiers);
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
		let tests = vec![
			("let x = 5;", "x", LiteralValue::Integer(5)),
			("let y = true;", "y", LiteralValue::Boolean(true)),
			("let foobar = y;", "foobar", LiteralValue::Identifier("y")),
		];

		for (input, expected_name, expected_value) in tests {
			let mut program = parse_statements(input, 1);
			let statement = program.statements.pop().unwrap();

			check_let_statement(statement.clone(), expected_name);
			
			match statement {
				Statement::Let(ast::LetStatement { value, .. }) => {
					check_literal_expression(value.unwrap(), expected_value);
				}

				_ => panic!("statement is not LetStatement. got={:?}", statement),
			}
		}
	}

	#[test]
	fn test_return_statements() {
		let tests = vec![
			("return 5;", LiteralValue::Integer(5)),
			("return true;", LiteralValue::Boolean(true)),
			("return foobar;", LiteralValue::Identifier("foobar")),
		];

		for (input, expected_value) in tests {
			let mut program = parse_statements(input, 1);
			let statement = program.statements.pop().unwrap();

			match statement {
				Statement::Return(ast::ReturnStatement { value, .. }) => {
					check_literal_expression(value.unwrap(), expected_value);
				}

				_ => panic!("statement is not ReturnStatement. got={:?}", statement),
			}
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
					panic!("boolean is not '{}'. got={}", value, boolean_value);
				}

				if token.literal != format!("{}", value) {
					panic!("boolean.token.literal is not '{}'. got={}", value, token.literal);
				}
			},

			_ => {
				panic!("expression is not BooleanLiteral. got={:?}", expression);
			}
		}
	}

	fn check_string_literal(expression: Expression, value: String) {
		match expression {
			Expression::StringLiteral(ast::StringLiteralExpression { token, value: string_value, .. }) => {
				if string_value != value {
					panic!("string is not '{}'. got={}", value, string_value);
				}

				if token.literal != format!("{}", value) {
					panic!("string.token.literal is not '{}'. got={}", value, token.literal);
				}
			},

			_ => {
				panic!("expression is not StringLiteral. got={:?}", expression);
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

	fn check_infix_expression(expression: Expression, left_value: LiteralValue, operator_literal: &str, right_value: LiteralValue) {
		match expression {
			Expression::Infix(ast::InfixExpression { operator, left, right }) => {
				assert_eq!(operator.literal, operator_literal);

				check_literal_expression(*left, left_value);
				check_literal_expression(*right, right_value);
			},

			_ => {
				panic!("expression is not Infix. got={:?}", expression);
			}
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

		for (input, expected_value) in tests {
			let expression = parse_expression(input);

			check_boolean_literal(expression, expected_value);
		}
	}

	#[test]
	fn test_parsing_string_literal_expression() {
		let input = "\"hello world\"";

		let expression = parse_expression(input);


		check_string_literal(expression, "hello world".to_string());
	}

	#[test]
	fn test_parsing_array_literal_expression() {
		let input = "[1, 2 * 5, 3 + 7]";

		let expression = parse_expression(input);

		match expression {
			Expression::ArrayLiteral(ast::ArrayLiteralExpression { mut elements, .. }) => {
				check_integer_literal(elements.remove(0), 1);
				check_infix_expression(elements.remove(0), LiteralValue::Integer(2), "*", LiteralValue::Integer(5));
				check_infix_expression(elements.remove(0), LiteralValue::Integer(3), "+", LiteralValue::Integer(7));
			}

			_ => panic!("expression is not ArrayLiteral. got={:?}", expression),
		}
	}

	#[test]
	fn test_parsing_prefix_expression() {
		let tests: Vec<(&str, &str, LiteralValue)> = vec![
			("!5;", "!", LiteralValue::Integer(5)),
			("-15;", "-", LiteralValue::Integer(15)),
		];

		for (input, operator_literal, right_value) in tests {
			let expression = parse_expression(input);

			match expression {
				Expression::Prefix(ast::PrefixExpression { operator, right }) => {
					assert_eq!(operator.literal, operator_literal);

					check_literal_expression(*right, right_value);
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

		for (input, expected_left, expected_operator, expected_right) in tests {
			let expression = parse_expression(input);

			check_infix_expression(expression, expected_left, expected_operator, expected_right);
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
			(
				"a + add(b * c) + d",
				"((a + add((b * c))) + d)",
			),
			(
				"add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
				"add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
			),
			(
				"add(a + b + c * d / f + g)",
				"add((((a + b) + ((c * d) / f)) + g))",
			),
			(
				"a * [1, 2, 3, 4][b * c] * d",
				"((a * ([1, 2, 3, 4][(b * c)])) * d)",
			),
			(
				"add(a * b[2], b[1], 2 * [1, 2][1])",
				"add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
			),
		];

		for (input, expected_program) in tests {
			let program = parse(input);

			assert_eq!(program.to_string(), expected_program);
		}
	}

	#[test]
	fn test_parsing_if_expression() {
		let input = "if (x < y) { x }";

		let expression = parse_expression(input);

		match expression {
			Expression::If(ast::IfExpression { condition, mut consequence, alternative, .. }) => {
				check_infix_expression(*condition, LiteralValue::Identifier("x"), "<", LiteralValue::Identifier("y"));

				let statement_count = consequence.statements.len();
				assert_eq!(statement_count, 1, "consequence statements length is not 1");

				let statement = consequence.statements.pop();
				match statement {
					Some(Statement::Expression(ast::ExpressionStatement { expression, .. })) => {
						check_identifier(expression, "x");
					}
					_ => panic!("consequece.statements[0] is not Expression. got={:?}", statement),
				}

				if let Some(alternative) = alternative {
					panic!("alternative is not None. got={:?}", alternative);
				}
			},

			_ => {
				panic!("expression is not IfExpression. got={:?}", expression);
			}
		}
	}

	#[test]
	fn test_parsing_if_else_expression() {
		let input = "if (x < y) { x } else { y }";

		let expression = parse_expression(input);

		match expression {
			Expression::If(ast::IfExpression { condition, mut consequence, alternative, .. }) => {
				check_infix_expression(*condition, LiteralValue::Identifier("x"), "<", LiteralValue::Identifier("y"));

				let statement_count = consequence.statements.len();
				assert_eq!(statement_count, 1, "consequence statements length is not 1");

				let statement = consequence.statements.pop();
				match statement {
					Some(Statement::Expression(ast::ExpressionStatement { expression, .. })) => {
						check_identifier(expression, "x");
					}
					_ => panic!("consequece.statements[0] is not Expression. got={:?}", statement),
				}

				match alternative {
					Some(mut alternative) => {
						let statement_count = alternative.statements.len();
						assert_eq!(statement_count, 1, "altenative statements length is not 1");

						let statement = alternative.statements.pop();
						match statement {
							Some(Statement::Expression(ast::ExpressionStatement { expression, .. })) => {
								check_identifier(expression, "y");
							}
							_ => panic!("alternative.statements[0] is not Expression. got={:?}", statement),
						}

					}

					None => panic!("alternative is None"),
				}
			},

			_ => {
				panic!("expression is not IfExpression. got={:?}", expression);
			}
		}
	}

	#[test]
	fn test_parsing_function_expression() {
		let input = "fn(x, y) { x + y; }";

		let expression = parse_expression(input);

		match expression {
			Expression::Function(ast::FunctionExpression { parameters, mut body , .. }) => {
				if parameters.len() != 2 {
					panic!("function parameters wrong. want 2, got={}", parameters.len());
				}

				if parameters[0].value != "x" {
					panic!("parameter[0] is not x. got={}", parameters[0].value);
				}

				if parameters[1].value != "y" {
					panic!("parameter[1] is not y. got={}", parameters[1].value);
				}

				if body.statements.len() != 1 {
					panic!("statements is not 1. got={}", body.statements.len());
				}

				let statement = body.statements.remove(0);
				match statement {
					Statement::Expression(ast::ExpressionStatement { expression, .. }) => {
						check_infix_expression(expression, LiteralValue::Identifier("x"), "+", LiteralValue::Identifier("y"));
					}
					
					_ => panic!("body statement is not Expression. got={:?}", statement),
				}
			}

			_ => {
				panic!("expression is not Function. got={:?}", expression);
			}
		}
	}

	#[test]
	fn test_parsing_call_expression() {
		let input = "add(1, 2* 3, 4 +5);";

		let expression = parse_expression(input);

		match expression {
			Expression::Call(ast::CallExpression { identifier, mut arguments, .. }) => {
				check_identifier(*identifier, "add");

				let args_count = arguments.len();
				if args_count != 3 {
					panic!("wrong number of arguments. got={}", args_count);
				}

				let arg0 = arguments.remove(0);
				check_literal_expression(arg0, LiteralValue::Integer(1));

				let arg1 = arguments.remove(0);
				check_infix_expression(arg1, LiteralValue::Integer(2), "*", LiteralValue::Integer(3));

				let arg2 = arguments.remove(0);
				check_infix_expression(arg2, LiteralValue::Integer(4), "+", LiteralValue::Integer(5));
			}

			_ => panic!("expression is not CallExpression. got={:?}", expression),
		}
	}

	#[test]
	fn test_parsing_index_expression() {
		let input = "myArray[1 + 1]";
	
		let expression = parse_expression(input);

		match expression {
			Expression::Index(ast::IndexExpression { left, index , .. }) => {
				check_identifier(*left, "myArray");
				check_infix_expression(*index, LiteralValue::Integer(1), "+", LiteralValue::Integer(1));
			}

			_ => panic!("expression is not IndexExpression. got={:?}", expression),
		}
	}

	#[test]
	fn test_parsing_hash_literal_empty_expression() {
		let input = "{}";

		let expression = parse_expression(input);

		match expression {
			Expression::HashLiteral(ast::HashLiteralExpression { pairs, .. }) => {
				if pairs.len() != 0 {
					panic!("hash size is not empty. got={}", pairs.len());
				}
			}

			_ => panic!("expression is not ast.HashListeral. got={:?}", expression),
		}
	}

	#[test]
	fn test_parsing_hash_literal_expression() {
		let input = "{\"one\": 1, \"two\": 2, \"three\": 3 }";

		let expression = parse_expression(input);

		let expected: Vec<(&str, i64)> = vec![("one", 1), ("two", 2), ("three", 3)];

		match expression {
			Expression::HashLiteral(ast::HashLiteralExpression { pairs, .. }) => {
				
				for ((key_exp, value_exp), (expected_key, expected_value)) in pairs.into_iter().zip(expected.into_iter()) {
					check_string_literal(key_exp, expected_key.to_string());
					check_integer_literal(value_exp, expected_value);
				}
			}

			_ => panic!("expression is not HashLiteralExpression. got={:?}", expression),
		}
	}
	
}