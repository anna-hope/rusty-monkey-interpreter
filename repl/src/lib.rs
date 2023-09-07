use std::io::{self, Write};

use interpreter::prelude::{Lexer, Parser};

const PROMPT: &str = ">> ";

pub fn start() -> io::Result<()> {
    loop {
        print!("{PROMPT}");
        io::stdout().flush()?;

        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer)?;

        let lexer = Lexer::new(buffer);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        if !parser.errors.is_empty() {
            for error in parser.errors.iter() {
                println!("\t{error}");
            }
            continue;
        }

        println!("{program}");
    }
}
