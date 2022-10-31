use std::{cell::RefCell, rc::Rc};

use crate::{ast, ast::{Statement, Expression}, object::{Object, Environment}, token::TokenType};

pub struct Evaluator {
	env: Rc<RefCell<Environment>>
}

impl Evaluator {
	pub fn new() -> Evaluator {
		Evaluator {
			env: Rc::new(RefCell::new(Environment::new())),
		}
	}

	pub fn eval(&mut self, program: ast::Program) -> Object {
		let mut result: Object = Object::Null;
	
		for statement in program.statements {
			result = self.eval_statement(statement);
	
			match result {
				Object::ReturnValue(object) => return *object,
				Object::Error(_) => return result,
				_ => {}
			}
		}
	
		return result;
	}

	pub fn eval_statement(&mut self, statement: Statement) -> Object {
		match statement {
			Statement::Expression(ast::ExpressionStatement { expression, .. }) => self.eval_expression(expression),
	
			Statement::Return(ast::ReturnStatement { value, .. }) => {
				match value {
					Some(value) => {
						let right = self.eval_expression(value);
						if right.is_error() {
							return right;
						}
	
						return Object::ReturnValue(Box::new(right));
					}
					None => Object::ReturnValue(Box::new(Object::Null)),
				}
			}
	
			Statement::Let(ast::LetStatement { name, value, .. }) => {
				let value = value.map(|value| self.eval_expression(value)).unwrap_or(Object::Null);
				if value.is_error() {
					return value;
				}
	
				return self.env.borrow_mut().set(name.value, value);
			}
	
			Statement::Block(block_statement) => self.eval_block_statement(block_statement),
		}
	}

	pub fn eval_expression(&mut self, expression: Expression) -> Object {
		match expression {
			Expression::BooleanLiteral(ast::BooleanLiteralExpression { value, .. }) => Object::Boolean(value),
			Expression::IntegerLiteral(ast::IntegerLiteralExpression { value, .. }) => Object::Integer(value),
	
			
			Expression::Identifier(ast::IdentifierExpression { value: name, .. }) => {
				self.env.borrow().get(&name).unwrap_or(
					Object::Error(format!("identifier not found: {}", name))
				)
			}
	
			Expression::Prefix(ast::PrefixExpression { operator, right, .. }) => {
				let right = self.eval_expression(*right);
				if right.is_error() {
					return right;
				}
	
				return self.eval_expression_prefix(operator.ttype, right);
			}
	
			Expression::Infix(ast::InfixExpression { left, operator, right, .. }) => {
				let left = self.eval_expression(*left);
				if left.is_error() {
					return left;
				}
	
				let right = self.eval_expression(*right);
				if right.is_error() {
					return right;
				}
	
				return self.eval_expression_infix(operator.ttype, left, right);
			}
	
			Expression::If(ast::IfExpression { condition, consequence, alternative, .. }) => {
				let condition = self.eval_expression(*condition);
	
				if is_truthy(condition) {
					return self.eval_block_statement(consequence);
				} else if let Some(alternative) = alternative {
					return self.eval_block_statement(alternative);
				} else {
					return Object::Null;
				}
			}

			Expression::Function(ast::FunctionExpression { parameters, body, .. }) => {
				Object::Function { parameters, body, env: Rc::clone(&self.env) }
			}

			Expression::Call(ast::CallExpression { identifier, arguments, .. }) => {
				let function = self.eval_expression(*identifier);
				if function.is_error() {
					return function;
				}

				match self.eval_expressions(arguments) {
					Ok(arguments) => self.apply_function(function, arguments),
					Err(error) => error,
				}
			}
		}
	}

	fn eval_block_statement(&mut self, block_statement: ast::BlockStatement) -> Object {
		let mut result: Object = Object::Null;
	
		for statement in block_statement.statements {
			result = self.eval_statement(statement);
	
			match result {
				Object::ReturnValue(_) | Object::Error(_) => return result,
				_ => {}
			}
		}
	
		return result;
	}

	fn eval_expression_prefix(&self, operator: TokenType, right: Object) -> Object {
		match operator {
			TokenType::Bang => match right {
				Object::Null => Object::Boolean(true),
				Object::Boolean(value) => Object::Boolean(!value),
				_ => Object::Boolean(false),
			}
	
			TokenType::Minus => match right {
				Object::Integer(value) => Object::Integer(-value),
				_ => Object::Error(format!("unknown operator: {}{}", operator.inspect(), right.inspect_type())),
			}
	
	
			_ => Object::Error(format!("unknown operator: {}{}", operator.inspect(), right.inspect_type())),
		}
	}

	fn eval_expression_infix(&self, operator: TokenType, left: Object, right: Object) -> Object {
		match (left, right) {
			(Object::Integer(left), Object::Integer(right)) =>
				self.eval_expression_infix_integer(operator, left, right),
	
			(Object::Function { .. }, _) | (_, Object::Function { .. }) => Object::Boolean(false),
	
			(left, right) => {
				match operator {
					TokenType::Equal => Object::Boolean(left == right),
					TokenType::NotEqual => Object::Boolean(left != right),
	
					_ => {
						if std::mem::discriminant(&left) != std::mem::discriminant(&right) {
							return Object::Error(
								format!("type mismatch: {} {} {}", left.inspect_type(), operator.inspect(), right.inspect_type())
							);
	
						}
	
						return Object::Error(
							format!("unknown operator: {} {} {}", left.inspect_type(), operator.inspect(), right.inspect_type())
						);
					}
				}
	
			}
		}
	}

	fn eval_expression_infix_integer(&self, operator: TokenType, left: i64, right: i64) -> Object {
		match operator {
			// arithmetic
			TokenType::Plus => Object::Integer(left + right),
			TokenType::Minus => Object::Integer(left - right),
			TokenType::Asterisk => Object::Integer(left * right),
			TokenType::Slash => Object::Integer(left / right),
	
			// comparison
			TokenType::Equal => Object::Boolean(left == right),
			TokenType::NotEqual => Object::Boolean(left != right),
			TokenType::LowerThan => Object::Boolean(left < right),
			TokenType::GreaterThan => Object::Boolean(left > right),
	
			_ => Object::Error(
				format!("unknown operator: INTEGER {:?} INTEGER", operator.inspect())
			),
		}
	}

	fn eval_expressions(&mut self, expressions: Vec<Expression>) -> Result<Vec<Object>, Object> {
		let mut result = vec![];

		for expression in expressions {
			let evaluated = self.eval_expression(expression);
			if evaluated.is_error() {
				return Err(evaluated);
			}

			result.push(evaluated);
		}

		return Ok(result);
	}

	fn apply_function(&mut self, value: Object, args: Vec<Object>) -> Object {
		match value {
			Object::Function { env, parameters, body } => {
				let mut call_env = Environment::new_enclosed(&env);

				for (param, arg) in parameters.into_iter().zip(args.into_iter()) {
					call_env.set(param.value, arg);
				}

				let current_env = Rc::clone(&self.env);

				self.env = Rc::new(RefCell::new(call_env));
				let result = self.eval_block_statement(body);

				self.env = current_env;

				result.unwrap_return_value()
			}

			_ => Object::Error(format!("not a function: {}", value.inspect_type())),
		} 
	}
}


pub fn is_truthy(object: Object) -> bool {
	match object {
		Object::Null => false,
		Object::Boolean(value) => value,
		_ => true
	}
}

pub fn is_error(object: Object) -> bool {
	match object {
		Object::Error(_) => true,
		_ => false,
	}
}

#[cfg(test)]
mod tests {
    use crate::{object::Object, lexer::Lexer, parser::Parser};

    use super::Evaluator;

	fn check_eval(input: &str) -> Object {
		let mut lexer = Lexer::new(input);
		let mut parser = Parser::new(&mut lexer);

		let program = parser.parse_program();
		let mut evaluator = Evaluator::new();

		return evaluator.eval(program);
	}

	#[test]
	fn test_eval_boolean_expression() {
		let tests = vec![
			("true", true),
			("false", false),
			("1 < 2", true),
			("1 > 2", false),
			("1 < 1", false),
			("1 > 1", false),
			("1 == 1", true),
			("1 != 1", false),
			("1 == 2", false),
			("1 != 2", true),
			("true == true", true),
			("false == false", true),
			("true == false", false),
			("true != false", true),
			("false != true", true),
		];

		for (input, expected_value) in tests {
			let evaluated = check_eval(input);
			assert_eq!(evaluated, Object::Boolean(expected_value));
		}
	}

	#[test]
	fn test_eval_integer_expression() {
		let tests = vec![
			("5", 5),
			("10", 10),
			("-5", -5),
			("-10", -10),
			("5 + 5 + 5 + 5 - 10", 10),
			("2 * 2 * 2 * 2 * 2", 32),
			("-50 + 100 + -50", 0),
			("5 * 2 + 10", 20),
			("5 + 2 * 10", 25),
			("20 + 2 * -10", 0),
			("50 / 2 * 2 + 10", 60),
			("2 * (5 + 10)", 30),
			("3 * 3 * 3 + 10", 37),
			("3 * (3 * 3) + 10", 37),
			("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
		];

		for (input, expected_value) in tests {
			let evaluated = check_eval(input);
			assert_eq!(evaluated, Object::Integer(expected_value));
		}
	}

	#[test]
	fn test_eval_bang_operator() {
		let tests = vec![
			("!true", false),
			("!false", true),
			("!5", false),
			("!!true", true),
			("!!false", false),
			("!!5", true),
		];

		for (input, expected_value) in tests {
			let evaluated = check_eval(input);
			assert_eq!(evaluated, Object::Boolean(expected_value));
		}
	}

	#[test]
	fn test_eval_if_else_expression() {
		let tests = vec![
			("if (true) { 10 }", Object::Integer(10)),
			("if (false) { 10 }", Object::Null),
			("if (1) { 10 }", Object::Integer(10)),
			("if (1 < 2) { 10 }", Object::Integer(10)),
			("if (1 > 2) { 10 }", Object::Null),
			("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
			("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
		];

		for (input, expected_value) in tests {
			let evaluated = check_eval(input);
			assert_eq!(evaluated, expected_value);
		}
	}

	#[test]
	fn test_eval_return_statement() {
		let tests = vec![
			("return 10;", 10),
			("return 10; 9;", 10),
			("return 2 * 5; 9;", 10),
			("9; return 2 * 5; 9;", 10),
			(
				"
				if (10 > 1) {
					if (10 > 1) {
						return 10;
					}
					return 1;
				}
				",
				10,
			),
		];

		for (input, expected_value) in tests {
			let evaluated = check_eval(input);
			assert_eq!(evaluated, Object::Integer(expected_value));
		}
	}

	#[test]
	fn test_error_handling() {
		let tests = vec![
			(
				"5 + true;",
				"type mismatch: INTEGER + BOOLEAN",
			),
			(
				"5 + true; 5;",
				"type mismatch: INTEGER + BOOLEAN",
			),
			(
				"-true",
				"unknown operator: -BOOLEAN",
			),
			(
				"true + false;",
				"unknown operator: BOOLEAN + BOOLEAN",
			),
			(
				"5; true + false; 5",
				"unknown operator: BOOLEAN + BOOLEAN",
			),
			(
				"if (10 > 1) { true + false; }",
				"unknown operator: BOOLEAN + BOOLEAN",
			),
			(
				"
				if (10 > 1) {
				if (10 > 1) {
				return true + false;
				}
				return 1;
				}
				",
				"unknown operator: BOOLEAN + BOOLEAN",
			),
		];

		for (input, expected_value) in tests {
			let evaluated = check_eval(input);
			assert_eq!(evaluated, Object::Error(expected_value.to_string()));
		}
	}

	#[test]
	fn test_eval_let_statement() {
		let tests = vec![
			("let a = 5; a;", 5),
			("let a = 5 * 5; a;", 25),
			("let a = 5; let b = a; b;", 5),
			("let a = 5; let b = a; let c = a + b + 5; c;", 15),
		];

		for (input, expected_value) in tests {
			let evaluated = check_eval(input);
			assert_eq!(evaluated, Object::Integer(expected_value));
		}
	}

	#[test]
	fn test_eval_function() {
		let input = "fn(x) { x + 2; };";

		let evaluated = check_eval(input);

		match evaluated {
			Object::Function { parameters, body, .. } => {
				if parameters.len() != 1 {
					panic!("function has wrong parameters. Paramaters={}", parameters.len());
				}

				if parameters[0].value != "x" {
					panic!("parameter is not 'x'. got={:?}", parameters[0].value);
				}

				let body_str = body.to_string();
				let expected_body = "(x + 2)";

				if body_str != expected_body {
					panic!("body is not \"{}\". got={}", expected_body, body_str);
				}
			}

			_ => panic!("object is not a function. got={:?}", evaluated),
		}
	}

	#[test]
	fn test_function_application() {
		let tests = vec![
			("let identity = fn(x) { x; }; identity(5);", 5),
			("let identity = fn(x) { return x; }; identity(5);", 5),
			("let double = fn(x) { x * 2; }; double(5);", 10),
			("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
			("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
			("fn(x) { x; }(5)", 5),
		];

		for (input, expected_value) in tests {
			assert_eq!(check_eval(input), Object::Integer(expected_value));
		}
	}

	#[test]
	fn test_closures() {
		let input = "
			let newAdder = fn(x) {
				fn(y) { x + y; };
			}

			let addTwo = newAdder(2);
			addTwo(2);
		";

		assert_eq!(check_eval(input), Object::Integer(4));
	}
}