use std::io::{self, Write, Error};

use crate::{lexer::Lexer, token::TokenType};

const PROMPT: &str = ">> ";

pub struct Repl;

impl Repl {
	pub fn start(stdin: io::Stdin, stdout: &mut io::Stdout) -> Result<(), Error> {
		loop {
			write!(stdout, "{}", PROMPT)?;
			stdout.flush()?;

			let mut input = String::new();
			stdin.read_line(&mut input)?;

			let mut lexer = Lexer::new(input);

			let mut tok = lexer.next_token();
			while tok.ttype != TokenType::EOF {
				writeln!(stdout, "{:?}", tok)?;
				tok = lexer.next_token();
			}
		}
	}
}
