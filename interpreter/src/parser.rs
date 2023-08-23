use thiserror::Error;

use crate::ast::{Identifier, Program, Statement};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("Parser error: {0}")]
    Error(String),
}

pub type Result<T> = std::result::Result<T, ParserError>;

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();

        Self {
            lexer,
            current_token,
            peek_token,
            errors: vec![],
        }
    }

    fn advance(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn expect_peek(&mut self, token_type: TokenType) -> Result<()> {
        if self.peek_token.token_type == token_type {
            self.advance();
            Ok(())
        } else {
            let message = format!(
                "expected next token to be {}, got {} instead",
                token_type, self.peek_token.token_type
            );
            self.errors.push(message.clone());
            Err(ParserError::Error(message))
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while self.current_token.token_type != TokenType::Eof {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }
            self.advance();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.current_token.token_type {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => Some(self.parse_return_statement()),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let token = self.current_token.clone();

        if self.expect_peek(TokenType::Ident).is_err() {
            return None;
        }

        let name = Identifier::new(
            self.current_token.clone(),
            self.current_token.literal.clone(),
        );

        if self.expect_peek(TokenType::Assign).is_err() {
            return None;
        }

        while self.current_token.token_type != TokenType::Semicolon {
            self.advance();
        }

        Some(Statement::Let {
            token,
            name,
            value: None,
        })
    }

    fn parse_return_statement(&mut self) -> Statement {
        let token = self.current_token.clone();
        self.advance();

        // TODO: We're skipping the expressions until we
        // encounter a semicolon.
        while self.current_token.token_type != TokenType::Semicolon {
            self.advance();
        }

        Statement::Return { token, value: None }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Node, Statement};

    #[test]
    fn let_statements() {
        let input = r#"
let x = 5;
let y = 10;
let foobar = 838383;
"#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let had_errors = check_errors(&parser);
        assert!(!had_errors);

        assert_eq!(
            program.statements.len(),
            3,
            "Program should contain 3 statements"
        );

        let tests = ["x", "y", "foobar"];

        for (statement, expected_name) in program.statements.iter().zip(tests) {
            assert_eq!(statement.token_literal(), "let".to_string());
            match statement {
                Statement::Let { name, .. } => {
                    assert_eq!(name.value, expected_name.to_string());
                    assert_eq!(name.token_literal(), expected_name.to_string());
                }
                _ => panic!("Unexpected statement variant!"),
            }
        }
    }

    #[test]
    fn let_statements_errors() {
        let input = r#"
let x 5;
let = 10;
let 838383;
"#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let _program = parser.parse_program();
        let had_errors = check_errors(&parser);
        assert!(had_errors);
    }

    fn check_errors(parser: &Parser) -> bool {
        if parser.errors.is_empty() {
            return false;
        }

        eprintln!("parser has {} errors", parser.errors.len());
        for error in parser.errors.iter() {
            eprintln!("parser error: {error}")
        }

        true
    }

    #[test]
    fn return_statements() {
        let input = r#"
return 5;
return 10;
return 993322;
"#;
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let had_errors = check_errors(&parser);

        assert!(!had_errors);

        assert_eq!(
            program.statements.len(),
            3,
            "Program should contain 3 statements"
        );

        for statement in program.statements {
            match statement {
                Statement::Return { .. } => {
                    assert_eq!(statement.token_literal(), "return".to_string());
                }
                _ => panic!("Expected Statement::Return, got {statement:?}"),
            }
        }
    }
}
