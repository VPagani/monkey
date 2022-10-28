#[derive(Debug, PartialEq, Eq)]
pub enum Object {
	Null,
	Boolean(bool),
	Integer(i64),
	ReturnValue(Box<Object>),
	Error(String),
}

impl Object {
	pub fn inspect(&self) -> String {
		match self {
			Object::Null => "null".to_string(),
			Object::Boolean(value) => format!("{}", value),
			Object::Integer(value) => format!("{}", value),
			Object::ReturnValue(object) => object.inspect(),
			Object::Error(message) => format!("ERROR: {}", message),
		}
	}

	
	pub fn inspect_type(&self) -> &str {
		match self {
			Object::Null => "NULL",
			Object::Boolean(_) => "BOOLEAN",
			Object::Integer(_) => "INTEGER",
			Object::ReturnValue(_) => "RETURN_VALUE",
			Object::Error(_) => "ERROR",
		}
	}

	pub fn is_error(&self) -> bool {
		match self {
			Object::Error(_) => true,
			_ => false,
		}
	}
}