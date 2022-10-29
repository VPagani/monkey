use std::io::{self, Write, Error};

use crate::{
	token::TokenType,
	lexer::Lexer,
	parser::Parser,
	object::Environment,
	eval::eval
};

const PROMPT: &str = ">> ";

#[allow(dead_code)]
enum ReplType {
	RLPL,
	RPPL,
	REPL,
}

const REPL_TYPE: ReplType = ReplType::REPL;

pub struct Repl {
	environment: Environment,
}

impl Repl {
	pub fn new() -> Self {
		Repl {
			environment: Environment::default(),
		}
	}

	pub fn start(&mut self, stdin: io::Stdin, stdout: &mut io::Stdout) -> Result<(), Error> {
		loop {
			write!(stdout, "{}", PROMPT)?;
			stdout.flush()?;

			let mut input = String::new();
			stdin.read_line(&mut input)?;

			let mut lexer = Lexer::new(input.as_str());

			match REPL_TYPE {
				ReplType::RLPL => {
					self.print_program_tokens(stdout, &mut lexer)?;
				}

				ReplType::RPPL => {
					self.print_program_parsed(stdout, &mut lexer)?;
				}

				ReplType::REPL => {
					self.print_program_evaluated(stdout, &mut lexer)?;
				}
			}
		}
	}

	fn print_program_tokens(&self, stdout: &mut io::Stdout, lexer: &mut Lexer) -> Result<(), Error> {
		let mut tok = lexer.next_token();
		while tok.ttype != TokenType::EOF {
			writeln!(stdout, "{:?}", tok)?;
			tok = lexer.next_token();
		}

		return Ok(());
	}

	fn print_program_parsed<'a>(&self, stdout: &mut io::Stdout, lexer: &'a mut Lexer<'a>) -> Result<(), Error> {
		let mut parser: Parser<'a> = Parser::new(lexer);
		let program = parser.parse_program();

		if parser.errors.len() != 0 {
			self.print_parser_errors(stdout, parser.errors)?;
			return Ok(());
		}

		writeln!(stdout, "{}", program.to_string())?;

		return Ok(());
	}

	fn print_program_evaluated<'a>(&mut self, stdout: &mut io::Stdout, lexer: &'a mut Lexer<'a>) -> Result<(), Error> {
		let mut parser: Parser<'a> = Parser::new(lexer);
		let program = parser.parse_program();

		if parser.errors.len() != 0 {
			self.print_parser_errors(stdout, parser.errors)?;
			return Ok(());
		}

		let evaluated = eval(program, &mut self.environment);
		writeln!(stdout, "{}", evaluated.inspect())?;

		return Ok(());
	}

	fn print_parser_errors(&self, stdout: &mut io::Stdout, errors: Vec<String>) -> Result<(), Error> {
		write!(stdout, "{}", MONKEY_FACE)?;
		writeln!(stdout, "Woops! We ran into some monkey business here!")?;
		writeln!(stdout, " parser errors:")?;

		for msg in errors {
			writeln!(stdout, "\t{}", msg)?;
		}

		return Ok(());
	}
}

const MONKEY_FACE: &str = "              __,__
     .--.  .-\"     \"-.  .--.
    / .. \\/  .-. .-.  \\/ .. \\
   | |  '|  /   Y   \\  |'  | |
   | \\   \\  \\ 0 | 0 /  /   / |
    \\ '- ,\\.-\"\"\"\"\"\"\"-./, -' /
     ''-' /_   ^ ^   _\\ '-''
         |  \\._   _./  |
         \\   \\ '~' /   /
          '._ '-=-' _.'
             '-----'
";
