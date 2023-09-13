use std::io::{self, Write};
use std::process;
use std::sync::Arc;

use interpreter::prelude::{add_environment, eval_program, Environment, Lexer, Parser};

const PROMPT: &str = ">> ";

pub fn start() -> io::Result<()> {
    let environment = Arc::new(Environment::new());
    let environment_key = add_environment(&environment);

    loop {
        print!("{PROMPT}");
        io::stdout().flush()?;

        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer)?;

        if buffer.trim() == "quit" {
            process::exit(0);
        }

        let lexer = Lexer::new(buffer);
        let mut parser = Parser::new(lexer);

        match parser.parse_program() {
            Ok(program) => {
                let evaluated = eval_program(&program, environment_key);
                println!("{}", evaluated.inspect());
            }
            Err(error) => println!("{error}"),
        }
    }
}
