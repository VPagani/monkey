pub struct Program {
	pub statements: Vec<Statement>,
}

trait Node {
	fn literal(self) -> String;
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
	Identifier(String),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
	Let{ name: String },
}

impl Program {
	pub fn new() -> Program {
		Program {
			statements: Vec::new()
		}
	}
}

impl Node for Expression {
	fn literal(self) -> String {
		use Expression::*;

		match self {
			Identifier(name) => name,
		}
	}
}

impl Node for Statement {
	fn literal(self) -> String {
		use Statement::*;

		match self {
			Let { name } => name,
		}
	}
}
