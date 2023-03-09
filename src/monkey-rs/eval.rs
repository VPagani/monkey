use std::{cell::RefCell, rc::Rc, collections::HashMap};

use crate::{
	ast,
	ast::{Node, Statement, Expression, AstNode},
	object::{Object, Environment, BuiltinFunction, ObjectFunction},
	token::TokenType
};

pub struct Evaluator {
	pub env: RefCell<Rc<RefCell<Environment>>>
}

impl Evaluator {
	pub fn new() -> Evaluator {
		Evaluator {
			env: RefCell::new(Rc::new(RefCell::new(Environment::new()))),
		}
	}

	pub fn eval(&self, program: ast::Program) -> Object {
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

	pub fn eval_statement(&self, statement: Statement) -> Object {
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
	
				return self.env.borrow().borrow_mut().set(name.value, value);
			}
	
			Statement::Block(block_statement) => self.eval_block_statement(block_statement),
		}
	}

	pub fn eval_expression(&self, expression: Expression) -> Object {
		match expression {
			Expression::NullLiteral => Object::Null,
			Expression::BooleanLiteral(ast::BooleanLiteralExpression { value, .. }) => Object::Boolean(value),
			Expression::IntegerLiteral(ast::IntegerLiteralExpression { value, .. }) => Object::Integer(value),
			Expression::StringLiteral(ast::StringLiteralExpression { value, .. }) => Object::String(value),

			Expression::ArrayLiteral(ast::ArrayLiteralExpression { elements, .. }) => {
				let elements = self.eval_expressions(elements);
				match elements {
					Err(error) => error,
					Ok(elements) => Object::Array(elements),
				}
			}
			
			Expression::HashLiteral(ast::HashLiteralExpression { pairs,  .. }) => {
				let mut hash_pairs = HashMap::new();

				for (key_node, value_node) in pairs {
					let key = self.eval_expression(key_node);
					if key.is_error() {
						return key;
					}

					let hash_key = match key.hash_key() {
						Some(hash_key) => hash_key,
						None => return Object::Error(
							format!("unusable as hash key: {}", key.inspect_type())
						)
					};

					let value = self.eval_expression(value_node);
					if value.is_error() {
						return value;
					}

					hash_pairs.insert(hash_key, (key, value));
				}

				return Object::Hash(hash_pairs);
			}

			Expression::ErrorLiteral(message) => Object::Error(message),

			Expression::Identifier(ast::IdentifierExpression { value: name, .. }) => {
				self.env.borrow().borrow().get(&name)
				.or_else(|| Object::builtin_function(&name))
				.unwrap_or(
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

			Expression::Macro(_) => Object::Null,

			Expression::Function(ast::FunctionExpression { parameters, body, .. }) => {
				Object::Function(ObjectFunction { parameters, body, env: Rc::clone(&self.env.borrow()) })
			}

			Expression::Call(ast::CallExpression { identifier, mut arguments, .. }) => {
				match identifier.as_ref() {
					Expression::Identifier(ast::IdentifierExpression { value, .. }) if value == "quote" => {
						return self.apply_quote_function(arguments.remove(0));
					}

					_ => {}
				}

				let function = self.eval_expression(*identifier);
				if function.is_error() {
					return function;
				}

				match self.eval_expressions(arguments) {
					Ok(arguments) => self.apply_function(function, arguments),
					Err(error) => error,
				}
			}

			Expression::Index(ast::IndexExpression { left, index, .. }) => {
				let left = self.eval_expression(*left);
				if left.is_error() {
					return left;
				}

				let index = self.eval_expression(*index);
				if index.is_error() {
					return index;
				}

				match (left, index) {
					(Object::Array(array), Object::Integer(index)) =>
						array
							.get(index as usize)
							.map(|value| value.clone())
							.unwrap_or(Object::Null),

					(Object::Hash(hash), index) => {
						let index = match index.hash_key() {
							Some(index) => index,
							None => return Object::Error(format!("unusable as hash key: {}", index.inspect_type())),
						};

						return hash
							.get(&index)
							.map(|pair| pair.1.clone())
							.unwrap_or(Object::Null);
					}

					(left, _) => Object::Error(format!("index operator not supported: {}", left.inspect_type())),
				}
			}
		}
	}

	fn eval_block_statement(&self, block_statement: ast::BlockStatement) -> Object {
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

			(Object::String(left), Object::String(right)) if operator == TokenType::Plus =>
				Object::String(format!("{}{}", left, right)),
	
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
			TokenType::LowerThanOrEqual => Object::Boolean(left <= right),
			TokenType::GreaterThanOrEqual => Object::Boolean(left >= right),
	
			_ => Object::Error(
				format!("unknown operator: INTEGER {:?} INTEGER", operator.inspect())
			),
		}
	}

	fn eval_expressions(&self, expressions: Vec<Expression>) -> Result<Vec<Object>, Object> {
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

	fn apply_function(&self, value: Object, args: Vec<Object>) -> Object {
		match value {
			Object::Function(ObjectFunction { env, parameters, body }) => {
				let mut call_env = Environment::new_enclosed(&env);

				for (param, arg) in parameters.into_iter().zip(args.into_iter()) {
					call_env.set(param.value, arg);
				}

				let current_env = Rc::clone(&self.env.borrow());
				{
					*self.env.borrow_mut() = Rc::new(RefCell::new(call_env));
				}
				let result = self.eval_block_statement(body);

				{
					*self.env.borrow_mut() = current_env;
				}

				result.unwrap_return_value()
			}

			Object::Builtin(builtin) => {
				self.apply_builtin_function(builtin, args)
			}

			_ => Object::Error(format!("not a function: {}", value.inspect_type())),
		} 
	}

	fn apply_builtin_function(&self, builtin: BuiltinFunction, mut args: Vec<Object>) -> Object {
		match builtin {
			BuiltinFunction::Len => {
				match &args[..] {
					[arg0] => {
						match arg0 {
							Object::String(value) => Object::Integer(value.len() as i64),
							Object::Array(array) => Object::Integer(array.len() as i64),
							_ => Object::Error(format!("argument to 'len' not supported, got {}", arg0.inspect_type()))
						}
					}

					_ => Object::Error(format!("wrong number of arguments. got={}, want=1", args.len()))
				}
			}

			BuiltinFunction::First => {
				match &args[..] {
					[arg0] => {
						match arg0 {
							Object::Array(array) => array.first().map(|value| value.clone()).unwrap_or(Object::Null),
							_ => Object::Error(format!("argument to `first` must be ARRAY, got {}", arg0.inspect_type())),
						}
					}

					_ => Object::Error(format!("wrong number of arguments. got={}, want=1", args.len()))
				}
			}

			BuiltinFunction::Last => {
				match &args[..] {
					[arg0] => {
						match arg0 {
							Object::Array(array) => array.last().map(|value| value.clone()).unwrap_or(Object::Null),
							_ => Object::Error(format!("argument to `last` must be ARRAY, got {}", arg0.inspect_type())),
						}
					}

					_ => Object::Error(format!("wrong number of arguments. got={}, want=1", args.len()))
				}
			}

			BuiltinFunction::Rest => {
				match &args[..] {
					[arg0] => {
						match arg0 {
							Object::Array(array) => Object::Array(Vec::from_iter(array[1..].iter().cloned())),
							_ => Object::Error(format!("argument to `rest` must be ARRAY, got {}", arg0.inspect_type())),
						}
					}

					_ => Object::Error(format!("wrong number of arguments. got={}, want=1", args.len()))
				}
			}

			BuiltinFunction::Push => {
				match &mut args[..] {
					[arg0, arg1] => {
						match arg0 {
							Object::Array(array) => {
								let mut array = array.clone();
								array.push(arg1.clone());
								return Object::Array(array);
							}
							_ => Object::Error(format!("argument to `rest` must be ARRAY, got {}", arg0.inspect_type())),
						}
					}

					_ => Object::Error(format!("wrong number of arguments. got={}, want=1", args.len()))
				}
			}

			BuiltinFunction::Puts => {
				for (i, arg) in args.iter().enumerate() {
					if i != 0 {
						print!(" ");
					}

					print!("{}", arg.inspect());
				}

				println!("");

				return Object::Null;
			}
		}
	}

	fn apply_quote_function(&self, ast: ast::Expression) -> Object {
		let node = AstNode::Expression(ast).map(&|node|
			match node {
				AstNode::Expression(
					Expression::Call(ast::CallExpression { identifier, mut arguments, .. })
				) if identifier.as_ref().literal() == "unquote" && arguments.len() == 1 =>
					AstNode::Expression(
						self.eval_expression(arguments.remove(0)).to_expression()
					),
	
				_ => node,
			}
		);

		match node {
			AstNode::Expression(ast) =>
				Object::Quote(ast),
			
			_ => Object::Null,
		}
	}

	fn define_macros(&self, program: ast::Program) -> ast::Program {
		let mut statements: Vec<Statement> = vec![];

		for statement in program.statements {
			let Statement::Let(
				ast::LetStatement { name, value: Some(Expression::Macro(macro_expression)), .. }
			) = statement else {
				// if statement is not a macro definition, keep it
				statements.push(statement);
				continue;
			};

			let macro_object = Object::Macro(ObjectFunction {
				env: Rc::clone(&self.env.borrow()),
				parameters: macro_expression.parameters,
				body: macro_expression.body,
			});
			self.env.borrow().borrow_mut().set(name.value, macro_object);
		}

		return ast::Program {
			statements,
		};
	}

	fn get_macro(&self, call_expression: &ast::CallExpression) -> Option<ObjectFunction> {
		let Expression::Identifier(ast::IdentifierExpression { value: identifier, .. }) = call_expression.identifier.as_ref() else {
			return None;
		};

		let Object::Macro(macro_function) = self.env.borrow().borrow().get(identifier)? else {
			return None;
		};

		return Some(macro_function);
	}

	fn expand_macros(&self, program: ast::Program) -> ast::Program {
		let node = AstNode::Program(program).map(&|node|
			match node {
				AstNode::Expression(Expression::Call(ref call_expression)) => {
					let Some(macro_fn) = self.get_macro(call_expression) else {
						return node;
					};

					let args = call_expression.arguments.iter().map(|arg| Object::Quote(arg.clone())).collect::<Vec<_>>();
					let mut env = Environment::new_enclosed(&macro_fn.env);

					for (param, arg) in macro_fn.parameters.iter().zip(args.into_iter()) {
						env.set(param.value.clone(), arg);
					}

					let current_env = Rc::clone(&self.env.borrow());
					{
						*self.env.borrow_mut() = Rc::new(RefCell::new(env));
					}

					let Object::Quote(quote) = self.eval_block_statement(macro_fn.body) else {
						panic!("we only support returning AST-node from macros");
					};

					{
						*self.env.borrow_mut() = current_env;
					}

					return AstNode::Expression(quote);
				}
				_ => node
			}
		);

		match node {
			AstNode::Program(program) => program,
			_ => unreachable!(),
		}
	}

	pub fn handle_macros(&self, program: ast::Program) -> ast::Program {
		return self.expand_macros(self.define_macros(program));
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
	use crate::{
		ast::Node,
		ast::Program,
		lexer::Lexer,
		parser::Parser,
		object::Object,
		object::ObjectFunction,
	};

	use super::Evaluator;

	fn check_program(input: &str) -> Program {
		let mut lexer = Lexer::new(input);
		let mut parser = Parser::new(&mut lexer);

		let program = parser.parse_program();
		if parser.errors.len() > 0 {
			for error in parser.errors {
				panic!("{}", error);
			}
		}

		return program;
	}

	fn check_program_eval(input: &str) -> Object {
		let program = check_program(input);
		let evaluator = Evaluator::new();

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
			let evaluated = check_program_eval(input);
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
			let evaluated = check_program_eval(input);
			assert_eq!(evaluated, Object::Integer(expected_value));
		}
	}

	#[test]
	fn test_eval_string_expression() {
		let input = "\"hello world\"";

		let evaluated = check_program_eval(input);
		assert_eq!(evaluated, Object::String("hello world".to_string()));
	}

	#[test]
	fn test_string_concatenation() {
		let input = "\"Hello\" + \" \" + \"World!\"";

		let evaluated = check_program_eval(input);
		assert_eq!(evaluated, Object::String("Hello World!".to_string()));
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
			let evaluated = check_program_eval(input);
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
			let evaluated = check_program_eval(input);
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
			let evaluated = check_program_eval(input);
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
			(
				"{\"name\": \"Monkey\"}[fn(x) { x }];",
				"unusable as hash key: FUNCTION",
			),
		];

		for (input, expected_value) in tests {
			let evaluated = check_program_eval(input);
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
			let evaluated = check_program_eval(input);
			assert_eq!(evaluated, Object::Integer(expected_value));
		}
	}

	#[test]
	fn test_eval_function() {
		let input = "fn(x) { x + 2; };";

		let evaluated = check_program_eval(input);

		match evaluated {
			Object::Function(ObjectFunction { parameters, body, .. }) => {
				if parameters.len() != 1 {
					panic!("function has wrong parameters. Paramaters={}", parameters.len());
				}

				if parameters[0].value != "x" {
					panic!("parameter is not 'x'. got={:?}", parameters[0].value);
				}

				let body_str = body.to_string();
				let expected_body = "{ (x + 2) }";

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
			assert_eq!(check_program_eval(input), Object::Integer(expected_value));
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

		assert_eq!(check_program_eval(input), Object::Integer(4));
	}

	#[test]
	fn test_builtin_functions() {
		let tests = vec![
			("len(\"\")", Object::Integer(0)),
			("len(\"four\")", Object::Integer(4)),
			("len(\"hello world\")", Object::Integer(11)),
			("len(1)", Object::Error("argument to 'len' not supported, got INTEGER".to_string())),
			("len(\"one\", \"two\")", Object::Error("wrong number of arguments. got=2, want=1".to_string())),
		];

		for (input, expected_value) in tests {
			assert_eq!(check_program_eval(input), expected_value)
		}
	}

	#[test]
	fn test_eval_array_literals() {
		let input = "[1, 2 * 4, 3 + 8]";

		let evaluated = check_program_eval(input);

		match evaluated {
			Object::Array(elements) => {
				if elements.len() != 3 {
					panic!("array has wrong num of elements. got={}", elements.len());
				}

				assert_eq!(elements[0], Object::Integer(1));
				assert_eq!(elements[1], Object::Integer(8));
				assert_eq!(elements[2], Object::Integer(11));
			}

			_ => panic!("object is not Array. got={:?}", evaluated),
		}
	}

	#[test]
	fn test_eval_array_index_expressions() {
		let tests = vec!(
			(
				"[1, 2, 3][0]",
				Object::Integer(1),
			),
			(
				"[1, 2, 3][1]",
				Object::Integer(2),
			),
			(
				"[1, 2, 3][2]",
				Object::Integer(3),
			),
			(
				"let i = 0; [1][i];",
				Object::Integer(1),
			),
			(
				"[1, 2, 3][1 + 1];",
				Object::Integer(3),
			),
			(
				"let myArray = [1, 2, 3]; myArray[2];",
				Object::Integer(3),
			),
			(
				"let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
				Object::Integer(6),
			),
			(
				"let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
				Object::Integer(2),
			),
			(
				"[1, 2, 3][3]",
				Object::Null,
			),
			(
				"[1, 2, 3][-1]",
				Object::Null,
			),
		);

		for (input, expected_value) in tests {
			assert_eq!(check_program_eval(input), expected_value);
		}
	}

	#[test]
	fn test_eval_hash() {
		let input = "
		let two = \"two\";
		{
			\"one\": 10 - 9,
			two: 1 + 1,
			\"thr\" + \"ee\": 6 / 2,
			4: 4,
			true: 5,
			false: 6,
		}
		";

		let evaluated = check_program_eval(input);

		let expected = vec![
			(Object::String("one".to_string()), 1),
			(Object::String("two".to_string()), 2),
			(Object::String("three".to_string()), 3),
			(Object::Integer(4), 4),
			(Object::Boolean(true), 5),
			(Object::Boolean(false), 6),
		];
		
		match evaluated {
			Object::Hash(pairs) => {
				if pairs.len() != expected.len() {
					panic!("Hash has wrong num of pairs. got={}", pairs.len());
				}

				for (expected_key, expected_value) in expected.into_iter() {
					let (key, value) = pairs.get(&expected_key.hash_key().unwrap()).unwrap();
					assert_eq!(*key, expected_key);
					assert_eq!(*value, Object::Integer(expected_value));
				}
			}

			_ => panic!("value is not Hash. got={:?}", evaluated),
		}
	}

	#[test]
	fn test_eval_hash_index_expression() {
		let tests = vec![
			(
				"{\"foo\": 5}[\"foo\"]",
				Object::Integer(5),
			),
			(
				"{\"foo\": 5}[\"bar\"]",
				Object::Null,
			),
			(
				"let key = \"foo\"; {\"foo\": 5}[key]",
				Object::Integer(5),
			),
			(
				"{}[\"foo\"]",
				Object::Null,
			),
			(
				"{5: 5}[5]",
				Object::Integer(5),
			),
			(
				"{true: 5}[true]",
				Object::Integer(5),
			),
			(
				"{false: 5}[false]",
				Object::Integer(5),
			),
		];

		for (input, expected_value) in tests {
			assert_eq!(check_program_eval(input), expected_value);
		}
	}

	#[test]
	fn test_eval_quote() {
		let tests = vec![
			("quote(5)", "5"),
			("quote(foobar)", "foobar"),
			("quote(foobar + barfoo)", "(foobar + barfoo)"),
		];

		for (input, expected) in tests {
			let evaluated = check_program_eval(input);
			
			match evaluated {
				Object::Quote(expression) => {
					assert_eq!(expression.to_string(), expected);
				}

				_ => panic!("expected Quote. got={:?}", evaluated),
			}
		}
	}

	#[test]
	fn test_eval_quote_unquote() {
		let tests = vec![
			("quote(unquote(4))", "4"),
			("quote(unquote(4 + 4))", "8"),
			("quote(8 + unquote(4 + 4))", "(8 + 8)"),
			("quote(unquote(4 + 4) + 8)", "(8 + 8)"),
			(
				"let foobar = 8;
				quote(foobar)",
				"foobar",
			),
			(
				"let foobar = 8;
				quote(unquote(foobar))",
				"8",
			),
			(
				"quote(unquote(true))",
				"true",
			),
			(
				"quote(unquote(true == false))",
				"false",
			),
			(
				"quote(unquote(quote(4 + 4)))",
				"(4 + 4)",
			),
			(
				"let quotedInfixExpression = quote(4 + 4);
				quote(unquote(4 + 4) + unquote(quotedInfixExpression))",
				"(8 + (4 + 4))",
			),

		];

		for (input, expected) in tests {
			let evaluated = check_program_eval(input);
			
			match evaluated {
				Object::Quote(expression) => {
					assert_eq!(expression.to_string(), expected);
				}

				_ => panic!("expected Quote. got={:?}", evaluated),
			}
		}
	}

	#[test]
	fn test_eval_macro_definition() {
		let input = "
		let number = 1;
		let function = fn(x, y) { x + y };
		let mymacro = macro(x, y) { x + y; };
		";

		let mut lexer = Lexer::new(input);
		let mut parser = Parser::new(&mut lexer);
		let program = parser.parse_program();

		let evaluator = Evaluator::new();
		let program = evaluator.define_macros(program);

		if program.statements.len() != 2 {
			panic!("wrong number of statements. got={}", program.statements.len());
		}

		if evaluator.env.borrow().borrow().has(&"number".to_string()) {
			panic!("number should not be defined");
		}

		if evaluator.env.borrow().borrow().has(&"function".to_string()) {
			panic!("function should not be defined");
		}

		let Some(object) = evaluator.env.borrow().borrow().get(&"mymacro".to_string()) else {
			panic!("macro should be defined");
		};

		let Object::Macro(ObjectFunction { parameters, body, .. }) = object else {
			panic!("object is not Macro. got={:?}", object);
		};

		assert_eq!(2, parameters.len(), "wrong number of parameters");

		assert_eq!("x", parameters[0].value);
		assert_eq!("y", parameters[1].value);

		assert_eq!("{ (x + y) }", body.to_string());
	}

	#[test]
	fn test_expand_macros() {
		let tests = vec![
			(
				"
				let infixExpression = macro() { quote(1 + 2); };

				infixExpression();
				",
				"(1 + 2)"
			),
			(
				"
				let reverse = macro(a, b) { quote(unquote(b) - unquote(a)); };
				reverse(2 + 2, 10 - 5);
				",
				"(10 - 5) - (2 + 2)"
			),
			(
				"
				let unless = macro(condition, consequence, alternative) {
					quote(if (!unquote(condition)) {
						unquote(consequence);
					} else {
						unquote(alternative);
					});
				};

				unless(10 > 5, puts(\"not greater\"), puts(\"greater\"));
				",
				"if (!(10 > 5)) { puts(\"not greater\") } else { puts(\"greater\") }"
			)
		];

		for (input, expected) in tests {
			let input_program = check_program(input);
			let expected_program = check_program(expected);

			let evaluator = Evaluator::new();
			let input_program = evaluator.handle_macros(input_program);

			assert_eq!(expected_program.to_string(), input_program.to_string());
		}
	}
}