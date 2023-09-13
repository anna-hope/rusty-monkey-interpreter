use std::io::{self, Write};
use std::process;

use interpreter::prelude::{eval_program, Lexer, Parser};

const PROMPT: &str = ">> ";

pub fn start() -> io::Result<()> {
    println!("This is the Monkey programming language!");
    println!("Feel free to type in commands");

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
                let evaluated = eval_program(&program);
                println!("{}", evaluated.inspect());
            }
            Err(error) => println!("{error}"),
        }
    }
}
