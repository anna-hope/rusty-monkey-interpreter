use std::env;
use std::fs;

use anyhow::{Error, Result};

use interpreter::prelude::*;

fn run_program(input: String) -> Result<()> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program()?;
    let evaluated = eval_program(&program);
    println!("{}", evaluated.inspect());

    Ok(())
}

fn main() -> Result<()> {
    if let Some(path) = env::args().nth(1) {
        let input = fs::read_to_string(path)?;
        run_program(input)
    } else {
        repl::start().map_err(Error::from)
    }
}
