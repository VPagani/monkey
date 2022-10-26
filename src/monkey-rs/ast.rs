use crate::token::{Token, TokenType};

pub trait Node {
	fn literal(&self) -> &str;
	fn to_string(&self) -> String;
}

pub struct Program {
	pub statements: Vec<Statement>,
}

// Statements

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
	Let(LetStatement),
	Return(ReturnStatement),
	Expression(ExpressionStatement),
	Block(BlockStatement),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LetStatement {
	pub token: Token,
	pub name: IdentifierExpression,
	pub value: Option<Expression>
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReturnStatement {
	pub token: Token,
	pub value: Option<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExpressionStatement {
	pub token: Token,
	pub expression: Expression,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockStatement {
	pub token: Token,
	pub statements: Vec<Statement>,
}


// Expressions

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression {
	Identifier(IdentifierExpression),
	BooleanLiteral(BooleanLiteralExpression),
	IntegerLiteral(IntegerLiteralExpression),
	Prefix(PrefixExpression),
	Infix(InfixExpression),
	If(IfExpression),
	Call(CallExpression),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IdentifierExpression {
	pub token: Token,
	pub value: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BooleanLiteralExpression {
	pub token: Token,
	pub value: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntegerLiteralExpression {
	pub token: Token,
	pub value: i64,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PrefixExpression {
	pub operator: Token,
	pub right: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InfixExpression {
	pub operator: Token,
	pub left: Box<Expression>,
	pub right: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IfExpression {
	pub token: Token,
	pub condition: Box<Expression>,
	pub consequence: BlockStatement,
	pub alternative: Option<BlockStatement>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CallExpression {
	pub token: Token,
	pub identifier: Box<Expression>,
	pub arguments: Vec<Expression>,
}

impl Program {
	pub fn new() -> Program {
		Program {
			statements: Vec::new()
		}
	}

	pub fn to_string(&self) -> String {
		let mut out = String::new();

		for statement in self.statements.as_slice() {
			out += statement.to_string().as_str();
		}

		return out;
	}
}

impl BlockStatement {
	pub fn to_string(&self) -> String {
		let mut out = String::new();

		for statement in self.statements.as_slice() {
			out += statement.to_string().as_str();
		}

		return out;
		
	}
}

impl Node for Statement {
	fn literal(&self) -> &str {
		use Statement::*;

		match self {
			Let(LetStatement { token, .. }) => token.literal.as_str(),
			Return(ReturnStatement { token, .. })  => token.literal.as_str(),
			Expression(ExpressionStatement { token, .. }) => token.literal.as_str(),
			Block(BlockStatement { token, .. }) => token.literal.as_str(),
		}
	}

	fn to_string(&self) -> String {
		use Statement::*;

		match self {
			Let(LetStatement { token, name, value }) => {
				let mut out = String::new();

				out += format!("{} ", token.literal).as_str();
				out += name.value.as_str();
				
				if let Some(expression) = value {
					out += " = ";
					out += expression.to_string().as_str();
				}

				out += ";";

				return out;
			},
			Return(ReturnStatement { token, value }) => {
				let mut out = String::new();

				out += token.literal.as_str();

				if let Some(expression) = &value {
					out += " ";
					out += expression.to_string().as_str();
				}

				out += ";";

				return out;
			},
			
			Expression(ExpressionStatement { expression, .. }) => {
				return expression.to_string();
			},

			Block(block_statement) => {
				block_statement.to_string()
			}
		}
	}
}

impl Expression {
	pub fn make_identifier(ident: &str) -> IdentifierExpression {
		IdentifierExpression {
			token: Token::str(TokenType::Ident, ident),
			value: ident.to_string(),
		}
	}
}

impl Node for Expression {
	fn literal(&self) -> &str {
		use Expression::*;

		return match self {
			Identifier(IdentifierExpression { value, .. }) => value.as_str(),
			BooleanLiteral(BooleanLiteralExpression { token, .. }) => token.literal.as_str(),
			IntegerLiteral(IntegerLiteralExpression { token, .. }) => token.literal.as_str(),
			Prefix(PrefixExpression { operator, .. }) => operator.literal.as_str(),
			Infix(InfixExpression { operator, .. }) => operator.literal.as_str(),
			If(IfExpression { token, .. }) => token.literal.as_str(),
			Call(CallExpression { token, .. }) => token.literal.as_str(),
		}
	}

	fn to_string(&self) -> String {
		use Expression::*;

		return match self {
			Identifier(IdentifierExpression { value, .. }) => value.clone(),
			BooleanLiteral(BooleanLiteralExpression { token, .. }) => token.literal.clone(),
			IntegerLiteral(IntegerLiteralExpression { token, .. }) => token.literal.clone(),
			Prefix(PrefixExpression { operator, right }) => format!("({}{})", operator.literal, right.to_string()),
			Infix(InfixExpression { left, operator, right }) => format!("({} {} {})", left.to_string(), operator.literal, right.to_string()),

			If(IfExpression { condition, consequence, alternative, .. }) => {
				let mut out = String::new();

				out += "if";
				out += condition.to_string().as_str();
				out += " ";
				out += consequence.to_string().as_str();

				if let Some(alternative) = alternative {
					out += "else ";
					out += alternative.to_string().as_str();
				}

				return out;
			}

			Call(CallExpression { identifier, arguments, .. }) => {
				let mut out = String::new();

				out += identifier.to_string().as_str();
				out += "(";
				out += arguments.iter().map(|arg| arg.to_string()).collect::<Vec<String>>().join(", ").as_str();
				out += ")";

				return out;
			}
		}
	}
}


#[cfg(test)]
mod tests {
	use super::*;
	use crate::token::TokenType;

	#[test]
	fn test_string() {
		let program = Program {
			statements: vec![
				Statement::Let(LetStatement {
					token: Token::str(TokenType::Let, "let"),
					name: Expression::make_identifier("myVar"),
					value: Some(Expression::Identifier(
						Expression::make_identifier("anotherVar")
					))
				})
			],
		};

		assert_eq!(program.to_string(), "let myVar = anotherVar;")
	}
}