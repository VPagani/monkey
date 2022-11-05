use crate::token::{Token, TokenType};

pub trait Node {
	fn literal(&self) -> &str;
	fn to_string(&self) -> String;
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
	NullLiteral,
	BooleanLiteral(BooleanLiteralExpression),
	IntegerLiteral(IntegerLiteralExpression),
	StringLiteral(StringLiteralExpression),
	ArrayLiteral(ArrayLiteralExpression),
	HashLiteral(HashLiteralExpression),
	ErrorLiteral(String),
	Prefix(PrefixExpression),
	Infix(InfixExpression),
	If(IfExpression),
	Function(FunctionExpression),
	Call(CallExpression),
	Index(IndexExpression),
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
pub struct StringLiteralExpression {
	pub token: Token,
	pub value: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArrayLiteralExpression {
	pub token: Token,
	pub elements: Vec<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HashLiteralExpression {
	pub token: Token,
	pub pairs: Vec<(Expression, Expression)>,
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
pub struct FunctionExpression {
	pub token: Token,
	pub parameters: Vec<IdentifierExpression>,
	pub body: BlockStatement,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CallExpression {
	pub token: Token,
	pub identifier: Box<Expression>,
	pub arguments: Vec<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IndexExpression {
	pub token: Token,
	pub left: Box<Expression>,
	pub index: Box<Expression>,
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

		out += "{ ";
		for statement in self.statements.as_slice() {
			out += statement.to_string().as_str();
		}
		out += " }";

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
			NullLiteral => "null",
			ErrorLiteral(_) => "error",
			BooleanLiteral(BooleanLiteralExpression { token, .. }) => token.literal.as_str(),
			IntegerLiteral(IntegerLiteralExpression { token, .. }) => token.literal.as_str(),
			StringLiteral(StringLiteralExpression { token, .. }) => token.literal.as_str(),
			ArrayLiteral(ArrayLiteralExpression { token, .. }) => token.literal.as_str(),
			HashLiteral(HashLiteralExpression { token, .. }) => token.literal.as_str(),
			Prefix(PrefixExpression { operator, .. }) => operator.literal.as_str(),
			Infix(InfixExpression { operator, .. }) => operator.literal.as_str(),
			If(IfExpression { token, .. }) => token.literal.as_str(),
			Function(FunctionExpression { token, .. }) => token.literal.as_str(),
			Call(CallExpression { token, .. }) => token.literal.as_str(),
			Index(IndexExpression { token, .. }) => token.literal.as_str(),
		}
	}

	fn to_string(&self) -> String {
		use Expression::*;

		return match self {
			Identifier(IdentifierExpression { value, .. }) => value.clone(),
			NullLiteral => "null".to_string(),
			BooleanLiteral(BooleanLiteralExpression { value, .. }) => format!("{}", value),
			IntegerLiteral(IntegerLiteralExpression { value, .. }) => format!("{}", value),
			StringLiteral(StringLiteralExpression { value, .. }) => value.clone(),
			ArrayLiteral(ArrayLiteralExpression { elements, .. }) => {
				let mut out = String::new();

				out += "[";
				out += elements.iter()
					.map(|el| el.to_string())
					.collect::<Vec<String>>()
					.join(", ").as_str();
				out += "]";

				return out;
			}

			HashLiteral(HashLiteralExpression { pairs, .. }) => {
				let mut out = String::new();

				out += "{";
				out += pairs.iter()
					.map(|(key, value)| format!("{}:{}", key.to_string(), value.to_string()))
					.collect::<Vec<String>>().join(", ").as_str();
				out += "}";

				return out;
			}

			ErrorLiteral(message) => format!("ERROR(\"{}\")", message),

			Prefix(PrefixExpression { operator, right }) =>
				format!("({}{})", operator.literal, right.to_string()),
			Infix(InfixExpression { left, operator, right }) =>
				format!("({} {} {})", left.to_string(), operator.literal, right.to_string()),

			If(IfExpression { condition, consequence, alternative, .. }) => {
				let mut out = String::new();

				out += "if(";
				out += condition.to_string().as_str();
				out += ") ";
				out += consequence.to_string().as_str();

				if let Some(alternative) = alternative {
					out += " else ";
					out += alternative.to_string().as_str();
				}

				return out;
			}

			Function(FunctionExpression { parameters, body, .. }) => {
				let mut out = String::new();

				out += "fn";
				out += "(";
				out += parameters.iter()
					.map(|param| param.value.clone())
					.collect::<Vec<String>>()
					.join(", ").as_str();
				out += ") ";
				out += body.to_string().as_str();

				return out;
			}

			Call(CallExpression { identifier, arguments, .. }) => {
				let mut out = String::new();

				out += identifier.to_string().as_str();
				out += "(";
				out += arguments.iter()
					.map(|arg| arg.to_string())
					.collect::<Vec<String>>()
					.join(", ").as_str();
				out += ")";

				return out;
			}

			Index(IndexExpression { left, index, ..}) => {
				let mut out = String::new();

				out += "(";
				out += left.to_string().as_str();
				out += "[";
				out += index.to_string().as_str();
				out += "])";

				return out;
			}
		}
	}
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AstNode {
	Program(Program),
	Statement(Statement),
	Expression(Expression),
}

impl AstNode {
	pub fn to_string(&self) -> String {
		match self {
			AstNode::Program(program) => program.to_string(),
			AstNode::Statement(statement) => statement.to_string(),
			AstNode::Expression(expression) => expression.to_string(),
		}
	}

	fn map_statement<F: Fn(Self) -> Self>(statement: Statement, map_fn: &F) -> Statement {
		match AstNode::Statement(statement.clone()).map(map_fn) {
			AstNode::Statement(statement) => statement,
			_ => statement,
		}
	}

	fn map_statements<F: Fn(Self) -> Self>(statements: Vec<Statement>, map_fn: &F) -> Vec<Statement> {
		statements.into_iter()
			.map(|statement| AstNode::map_statement(statement, map_fn))
			.collect()
	}

	fn map_block_statement<F: Fn(Self) -> Self>(block_statement: BlockStatement, map_fn: &F) -> BlockStatement {
		BlockStatement {
			token: block_statement.token,
			statements: AstNode::map_statements(block_statement.statements, map_fn),
		}
	}

	fn map_expression<F: Fn(Self) -> Self>(expression: Expression, map_fn: &F) -> Expression {
		match AstNode::Expression(expression.clone()).map(map_fn) {
			AstNode::Expression(expression) => expression,
			_ => expression,
		}
	}

	pub fn map<F: Fn(Self) -> Self>(self, map_fn: &F) -> AstNode {
		let node = match self {
			AstNode::Program(program) => {
				AstNode::Program(Program {
					statements: AstNode::map_statements(program.statements, map_fn),
				})
			}

			AstNode::Statement(statement) => AstNode::Statement(
				match statement {
					Statement::Expression(ExpressionStatement { token, expression }) =>
						Statement::Expression(ExpressionStatement {
							token,
							expression: AstNode::map_expression(expression, map_fn),
						}),

					Statement::Block(block_statement) =>
						Statement::Block(
							AstNode::map_block_statement(block_statement, map_fn)
						),

					Statement::Let(LetStatement { token, name, value }) =>
						Statement::Let(LetStatement {
							name,
							token,
							value: value.map(
								|value| AstNode::map_expression(value, map_fn)
							),
						}),

					Statement::Return(ReturnStatement { token, value }) =>
						Statement::Return(ReturnStatement {
							token,
							value: value.map(
								|value| AstNode::map_expression(value, map_fn)
							),
						}),
				
				}
			),

			AstNode::Expression(expression) => AstNode::Expression(
				match expression {
					Expression::ArrayLiteral(ArrayLiteralExpression { token, elements }) =>
						Expression::ArrayLiteral(ArrayLiteralExpression {
							token,
							elements: elements.into_iter().map(
								|element| AstNode::map_expression(element, map_fn)
							).collect(),
						}),

					Expression::HashLiteral(HashLiteralExpression { token, pairs }) =>
						Expression::HashLiteral(HashLiteralExpression {
							token,
							pairs: pairs.into_iter().map(
								|(key, value)|
									(
										AstNode::map_expression(key, map_fn),
										AstNode::map_expression(value, map_fn)
									)
							).collect(),
						}),

					Expression::Prefix(PrefixExpression { operator, right }) =>
						Expression::Prefix(PrefixExpression {
							operator,
							right: Box::new(AstNode::map_expression(*right, map_fn)),
						}),

					Expression::Infix(InfixExpression { operator, left, right }) =>
						Expression::Infix(InfixExpression {
							operator,
							left: Box::new(AstNode::map_expression(*left, map_fn)),
							right: Box::new(AstNode::map_expression(*right, map_fn)),
						}),

					Expression::Index(IndexExpression { token, left, index }) =>
						Expression::Index(IndexExpression {
							token,
							left: Box::new(AstNode::map_expression(*left, map_fn)),
							index: Box::new(AstNode::map_expression(*index, map_fn)),
						}),

					Expression::If(IfExpression { token, condition, consequence, alternative }) =>
						Expression::If(IfExpression {
							token,
							condition: Box::new(AstNode::map_expression(*condition, map_fn)),
							consequence: AstNode::map_block_statement(consequence, map_fn),
							alternative: alternative.map(
								|block| AstNode::map_block_statement(block, map_fn)
							),
						}),

					Expression::Function(FunctionExpression { token, parameters, body }) =>
						Expression::Function(FunctionExpression {
							token,
							parameters,
							body: AstNode::map_block_statement(body, map_fn),
						}),

					_ => expression,
				}
			),
		};

		return map_fn(node);
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

	#[test]
	fn test_map() {
		fn tok() -> Token {
			Token::str(TokenType::Illegal, "")
		}

		fn int(value: i64) -> Expression {
			return Expression::IntegerLiteral(
				IntegerLiteralExpression {
					token: tok(),
					value,
				}
			);
		}

		fn one() -> Expression { int(1) }
		fn two() -> Expression { int(2) }

		fn turn_one_into_two(node: AstNode) -> AstNode {
			match node {
				AstNode::Expression(Expression::IntegerLiteral(IntegerLiteralExpression { token, value })) if value == 1 =>
					AstNode::Expression(Expression::IntegerLiteral(IntegerLiteralExpression { token, value: 2 })),

				_ => node,
			}
		}

		let tests = vec![
			// Program
			(
				AstNode::Program(Program {
					statements: vec![
						Statement::Expression(ExpressionStatement {
							token: tok(),
							expression: one(),
						}),
					]
				}),
				AstNode::Program(Program {
					statements: vec![
						Statement::Expression(ExpressionStatement {
							token: tok(),
							expression: two(),
						}),
					]
				})
			),

			// Statements
			(
				AstNode::Statement(Statement::Return(ReturnStatement {
					token: Token::str(TokenType::Return, "return"),
					value: Some(one()),
				})),
				AstNode::Statement(Statement::Return(ReturnStatement {
					token: Token::str(TokenType::Return, "return"),
					value: Some(two()),
				})),
			),
			(
				AstNode::Statement(Statement::Let(LetStatement {
					token: Token::str(TokenType::Return, "let"),
					name: Expression::make_identifier("one"),
					value: Some(one()),
				})),
				AstNode::Statement(Statement::Let(LetStatement {
					token: Token::str(TokenType::Return, "let"),
					name: Expression::make_identifier("one"),
					value: Some(two()),
				})),
			),

			// Expressions
			(
				AstNode::Expression(one()),
				AstNode::Expression(two()),
			),
			(
				AstNode::Expression(Expression::ArrayLiteral(ArrayLiteralExpression {
					token: Token::char(TokenType::LBracket, '['),
					elements: vec![one(), one()],
				})),
				AstNode::Expression(Expression::ArrayLiteral(ArrayLiteralExpression {
					token: Token::char(TokenType::LBracket, '['),
					elements: vec![two(), two()],
				})),
			),
			(
				AstNode::Expression(Expression::HashLiteral(HashLiteralExpression {
					token: Token::char(TokenType::LBracket, '['),
					pairs: vec![
						(one(), one()),
						(one(), one()),
					],
				})),
				AstNode::Expression(Expression::HashLiteral(HashLiteralExpression {
					token: Token::char(TokenType::LBracket, '['),
					pairs: vec![
						(two(), two()),
						(two(), two()),
					],
				})),
			),
			(
				AstNode::Expression(Expression::Infix(InfixExpression {
					operator: Token::char(TokenType::Plus, '+'),
					left: Box::new(one()),
					right: Box::new(two()),
				})),
				AstNode::Expression(Expression::Infix(InfixExpression {
					operator: Token::char(TokenType::Plus, '+'),
					left: Box::new(two()),
					right: Box::new(two()),
				})),
			),
			(
				AstNode::Expression(Expression::Infix(InfixExpression {
					operator: Token::char(TokenType::Plus, '+'),
					left: Box::new(two()),
					right: Box::new(one()),
				})),
				AstNode::Expression(Expression::Infix(InfixExpression {
					operator: Token::char(TokenType::Plus, '+'),
					left: Box::new(two()),
					right: Box::new(two()),
				})),
			),
			(
				AstNode::Expression(Expression::Prefix(PrefixExpression {
					operator: Token::char(TokenType::Minus, '-'),
					right: Box::new(one()),
				})),
				AstNode::Expression(Expression::Prefix(PrefixExpression {
					operator: Token::char(TokenType::Minus, '-'),
					right: Box::new(two()),
				})),
			),
			(
				AstNode::Expression(Expression::Index(IndexExpression {
					token: Token::char(TokenType::LBracket, '['),
					left: Box::new(one()),
					index: Box::new(one())
				})),
				AstNode::Expression(Expression::Index(IndexExpression {
					token: Token::char(TokenType::LBracket, '['),
					left: Box::new(two()),
					index: Box::new(two()),
				})),
			),
			(
				AstNode::Expression(Expression::If(IfExpression {
					token: Token::str(TokenType::If, "if"),
					condition: Box::new(one()),
					consequence: BlockStatement {
						token: Token::char(TokenType::LBrace, '{'),
						statements: vec![
							Statement::Expression(ExpressionStatement {
								token: tok(),
								expression: one(),
							}),
						],
					},
					alternative: Some(BlockStatement {
						token: Token::char(TokenType::LBrace, '{'),
						statements: vec![
							Statement::Expression(ExpressionStatement {
								token: tok(),
								expression: one(),
							}),
						],
					}),
				})),
				AstNode::Expression(Expression::If(IfExpression {
					token: Token::str(TokenType::If, "if"),
					condition: Box::new(two()),
					consequence: BlockStatement {
						token: Token::char(TokenType::LBrace, '{'),
						statements: vec![
							Statement::Expression(ExpressionStatement {
								token: tok(),
								expression: two(),
							}),
						],
					},
					alternative: Some(BlockStatement {
						token: Token::char(TokenType::LBrace, '{'),
						statements: vec![
							Statement::Expression(ExpressionStatement {
								token: tok(),
								expression: two(),
							}),
						],
					}),
				})),
			),
			(
				AstNode::Expression(Expression::Function(FunctionExpression {
					token: Token::str(TokenType::Function, "fn"),
					parameters: vec![],
					body: BlockStatement {
						token: Token::char(TokenType::LBrace, '{'),
						statements: vec![
							Statement::Expression(ExpressionStatement {
								token: tok(),
								expression: one(),
							}),
						],
					}
				})),
				AstNode::Expression(Expression::Function(FunctionExpression {
					token: Token::str(TokenType::Function, "fn"),
					parameters: vec![],
					body: BlockStatement {
						token: Token::char(TokenType::LBrace, '{'),
						statements: vec![
							Statement::Expression(ExpressionStatement {
								token: tok(),
								expression: two(),
							}),
						],
					}
				})),
			)
		];

		for (input, expected) in tests {
			let modified = input.map(&turn_one_into_two);

			assert_eq!(modified.to_string(), expected.to_string());
		}
	}
}