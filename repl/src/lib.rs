use std::io::{self, Write};

use interpreter::prelude::Lexer;

const PROMPT: &str = ">> ";

pub fn start() -> io::Result<()> {
    loop {
        print!("{PROMPT}");
        io::stdout().flush()?;

        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer)?;

        let lexer = Lexer::new(buffer);

        for token in lexer {
            println!("{token:?}");
        }
    }
}
