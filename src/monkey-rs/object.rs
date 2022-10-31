use std::{collections::HashMap, cell::RefCell, rc::Rc};

use crate::ast;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Object {
	Null,
	Boolean(bool),
	Integer(i64),
	ReturnValue(Box<Object>),
	Function {
		parameters: Vec<ast::IdentifierExpression>,
		body: ast::BlockStatement,
		env: Rc<RefCell<Environment>>
	},
	Error(String),
}

impl Object {
	pub fn inspect(&self) -> String {
		match self {
			Object::Null => "null".to_string(),
			Object::Boolean(value) => format!("{}", value),
			Object::Integer(value) => format!("{}", value),
			Object::ReturnValue(object) => object.inspect(),
			Object::Function { parameters, body, .. } => {
				let mut out = String::new();

				out += "fn";
				out += "(";
				out += parameters.iter()
					.map(|param| param.value.clone())
					.collect::<Vec<String>>().join(", ").as_str();
				out += "(";
				out += body.to_string().as_str();

				return out;
			},
			Object::Error(message) => format!("ERROR: {}", message),
		}
	}

	
	pub fn inspect_type(&self) -> &str {
		match self {
			Object::Null => "NULL",
			Object::Boolean(_) => "BOOLEAN",
			Object::Integer(_) => "INTEGER",
			Object::ReturnValue(_) => "RETURN_VALUE",
			Object::Function { .. } => "FUNCTION",
			Object::Error(_) => "ERROR",
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
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
}