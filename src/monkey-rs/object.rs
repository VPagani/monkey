use std::{
	rc::Rc,
	cell::RefCell,
	hash::{Hasher},
	collections::{HashMap, hash_map::DefaultHasher},
};

use crate::{ast, ast::Node, token::{Token, TokenType}};

#[derive(Clone, Debug)]
pub enum Object {
	Null,
	Boolean(bool),
	Integer(i64),
	String(String),
	Array(Vec<Object>),
	Hash(HashMap<HashKey, (Object, Object)>),
	ReturnValue(Box<Object>),
	Macro(ObjectFunction),
	Function(ObjectFunction),
	Builtin(BuiltinFunction),
	Error(String),
	Quote(ast::Expression),
}

#[derive(Clone, Debug)]
pub struct ObjectFunction {
	pub parameters: Vec<ast::IdentifierExpression>,
	pub body: ast::BlockStatement,
	pub env: Rc<RefCell<Environment>>
}

impl Object {
	pub fn inspect(&self) -> String {
		match self {
			Object::Null => "null".to_string(),
			Object::Boolean(value) => format!("{}", value),
			Object::Integer(value) => format!("{}", value),
			Object::String(value) => value.clone(),

			Object::Array(elements) => {
				let mut out = String::new();

				out += "[";
				out += elements.iter()
					.map(|el| el.inspect())
					.collect::<Vec<String>>().join(", ").as_str();
				out += "]";

				return out;

			}

			Object::Hash(pairs) => {
				let mut out = String::new();

				out += "{";
				out += pairs.values()
					.map(|(key, value)| format!("{}:{}", key.inspect(), value.inspect()))
					.collect::<Vec<String>>().join(", ").as_str();
				out += "}";

				return out;
			}

			Object::ReturnValue(object) => object.inspect(),

			Object::Macro(ObjectFunction { parameters, body, .. }) => {
				let mut out = String::new();

				out += "macro(";
				out += parameters.iter()
					.map(|param| param.value.clone())
					.collect::<Vec<String>>().join(", ").as_str();
				out += ") {\n";
				out += body.to_string().as_str();
				out += "\n}";

				return out;
			}

			Object::Function(ObjectFunction { parameters, body, .. }) => {
				let mut out = String::new();

				out += "fn(";
				out += parameters.iter()
					.map(|param| param.value.clone())
					.collect::<Vec<String>>().join(", ").as_str();
				out += ") {\n";
				out += body.to_string().as_str();
				out += "\n}";

				return out;
			}

			Object::Builtin(_) => "builtin function".to_string(),
			Object::Error(message) => format!("ERROR: {}", message),
			Object::Quote(expression) => format!("QUOTE({})", expression.to_string()),
		}
	}

	
	pub fn inspect_type(&self) -> &str {
		match self {
			Object::Null => "NULL",
			Object::Boolean(_) => "BOOLEAN",
			Object::Integer(_) => "INTEGER",
			Object::String(_) => "STRING",
			Object::Array(_) => "ARRAY",
			Object::Hash(_) => "HASH",
			Object::ReturnValue(_) => "RETURN_VALUE",
			Object::Macro { .. } => "MACRO",
			Object::Function { .. } => "FUNCTION",
			Object::Builtin(_) => "BUILTIN",
			Object::Error(_) => "ERROR",
			Object::Quote(_) => "QUOTE",
		}
	}

	pub fn is_error(&self) -> bool {
		match self {
			Object::Error(_) => true,
			_ => false,
		}
	}

	pub fn is_truthy(&self) -> bool {
		match self {
			Object::Null => false,
			Object::Boolean(value) => *value == true,
			_ => true
		}
	}

	pub fn unwrap_return_value(self) -> Object {
		match self {
			Object::ReturnValue(value) => *value,
			_ => self,
		}
	}

	pub fn builtin_function(identifier: &String) -> Option<Object> {
		BuiltinFunction::from_identifier(identifier)
			.map(|builtin| Object::Builtin(builtin))
	}

	pub fn hash_key(&self) -> Option<HashKey> {
		match self {
			Object::Boolean(value) =>
				Some(HashKey {
					otype: self.inspect_type().to_string(),
					value: if *value { 1 } else { 0 },
				}),

			Object::Integer(value) =>
				Some(HashKey {
					otype: self.inspect_type().to_string(),
					value: *value as u64
				}),

			Object::String(value) => {
				let mut hasher = DefaultHasher::new();
				hasher.write(value.as_bytes());

				return Some(HashKey {
					otype: self.inspect_type().to_string(),
					value: hasher.finish(),
				});
			}

			_ => None,
		}
	}

	pub fn to_expression(self) -> ast::Expression {
		match self {
			Object::Boolean(value) => ast::Expression::BooleanLiteral(
				ast::BooleanLiteralExpression {
					token:
						if value {
							Token::str(TokenType::True, "true")
						} else {
							Token::str(TokenType::False, "false")
						},
					value,
				}
			),

			Object::Integer(value) => ast::Expression::IntegerLiteral(
				ast::IntegerLiteralExpression {
					token: Token::string(TokenType::Int, format!("{}", value)),
					value,
				}),

			Object::Error(message) => ast::Expression::ErrorLiteral(message),

			Object::Quote(expression) => expression,

			_ =>  ast::Expression::NullLiteral,
		}
	}
}

impl Eq for Object {}

impl PartialEq for Object {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Object::Null, Object::Null) => true,
			(Object::Boolean(value1), Object::Boolean(value2)) => value1 == value2,
			(Object::Integer(value1), Object::Integer(value2)) => value1 == value2,
			(Object::String(value1), Object::String(value2)) => value1 == value2,
			(Object::Builtin(value1), Object::Builtin(value2)) => value1 == value2,
			(Object::Error(message1), Object::Error(message2)) => message1 == message2,
			_ => false,
		}
	}
}


#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BuiltinFunction {
	Len,
	First,
	Last,
	Rest,
	Push,
	Puts,
}

impl BuiltinFunction {
	pub fn from_identifier(identifier: &String) -> Option<BuiltinFunction> {
		match identifier.as_str() {
			"len" => Some(BuiltinFunction::Len),
			"first" => Some(BuiltinFunction::First),
			"last" => Some(BuiltinFunction::Last),
			"rest" => Some(BuiltinFunction::Rest),
			"push" => Some(BuiltinFunction::Push),
			"puts" => Some(BuiltinFunction::Puts),
			_ => None,
		}
	}
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct HashKey {
	otype: String,
	value: u64,
}

#[derive(Clone, Debug)]
pub struct Environment {
	store: HashMap<String, Object>,
	outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
	pub fn new() -> Environment {
		Environment {
			store: HashMap::new(),
			outer: None,
		}
	}

	pub fn new_enclosed(outer: &Rc<RefCell<Environment>>) -> Environment {
		Environment {
			store: HashMap::new(),
			outer: Some(Rc::clone(outer)),
		}
	}

	pub fn get(&self, name: &String) -> Option<Object> {
		self.store
			.get(name)
			.map(|value| (*value).clone())
			.or_else(|| self.outer.as_ref().and_then(|env| env.borrow().get(name)))
	}

	pub fn set(&mut self, name: String, value: Object) -> Object {
		self.store.insert(name, value.clone());
		return value;
	}

	pub fn has(&self, name: &String) -> bool {
		self.store.contains_key(name) ||
		self.outer.as_ref().map_or(false, |env| env.borrow().has(name))
	}
}

#[cfg(test)]
mod tests {
    use super::Object;

	#[test]
	fn test_string_hash_key() {
		let hello1 = Object::String("Hello World".to_string());
		let hello2 = Object::String("Hello World".to_string());

		let diff1 = Object::String("My name is johnny".to_string());
		let diff2 = Object::String("My name is johnny".to_string());

		if hello1.hash_key() != hello2.hash_key() {
			panic!("strings with same content have different hash keys");
		}

		if diff1.hash_key() != diff2.hash_key() {
			panic!("strings with same content have different hash keys");
		}

		if hello1.hash_key() == diff1.hash_key() {
			panic!("strings with different content have same hash keys");
		}
	}
}