use std::{io, error::Error};

use monkey::repl::Repl;

fn main()  -> Result<(), Box<dyn Error>> {
    println!("");
    println!("Hello {}! This is the Monkey programming language!", whoami::username());
    println!("Fell free to type in commands\n");

    let mut repl = Repl::new();
    repl.start(io::stdin(), &mut io::stdout())?;

    Ok(())
}
