use std::collections::HashMap;
use thiserror::Error;

use crate::ast::{Expression, Identifier, IntegerLiteral, Program, Statement};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("Parser error: {0}")]
    Error(String),
}

pub type Result<T> = std::result::Result<T, ParserError>;

type PrefixParseFn = fn(&Parser) -> Result<Box<dyn Expression>>;
type InfixParseFn = fn(&Parser, Box<dyn Expression>) -> Result<Box<dyn Expression>>;

#[derive(Debug, PartialOrd, PartialEq)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,

    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();

        let mut prefix_parse_fns: HashMap<TokenType, PrefixParseFn> = HashMap::new();
        prefix_parse_fns.insert(TokenType::Ident, Self::parse_identifier);
        prefix_parse_fns.insert(TokenType::Int, Self::parse_integer_literal);

        Self {
            lexer,
            current_token,
            peek_token,
            errors: vec![],
            prefix_parse_fns,
            infix_parse_fns: HashMap::new(),
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
            _ => self.parse_expression_statement(),
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

        // TODO: We're skipping the expressions until we encounter a semicolon.
        while self.current_token.token_type != TokenType::Semicolon {
            self.advance();
        }

        Statement::Return { token, value: None }
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let token = self.current_token.clone();
        let expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token.token_type == TokenType::Semicolon {
            self.advance();
        }
        let statement = Statement::Expression { token, expression };
        Some(statement)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<dyn Expression>> {
        if let Some(prefix) = self.prefix_parse_fns.get(&self.current_token.token_type) {
            let maybe_left_expression = prefix(self);
            if let Ok(left_expression) = maybe_left_expression {
                Some(left_expression)
            } else {
                let error_message = maybe_left_expression.unwrap_err().to_string();
                self.errors.push(error_message);
                None
            }
        } else {
            None
        }
    }

    pub fn register_infix(&mut self, token_type: TokenType, function: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, function);
    }

    fn parse_identifier(&self) -> Result<Box<dyn Expression>> {
        Ok(Box::new(Identifier::new(
            self.current_token.clone(),
            self.current_token.literal.clone(),
        )))
    }

    fn parse_integer_literal(&self) -> Result<Box<dyn Expression>> {
        let token = self.current_token.clone();
        let value = token
            .literal
            .parse::<i64>()
            .map_err(|error| ParserError::Error(error.to_string()))?;
        Ok(Box::new(IntegerLiteral::new(token, value)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{IntegerLiteral, Node, Statement};

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

    #[test]
    fn identifier_expression() {
        let input = "foobar;";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let had_errors = check_errors(&parser);

        assert!(!had_errors);

        assert_eq!(
            program.statements.len(),
            1,
            "program should have 1 statement"
        );

        match &program.statements[0] {
            Statement::Expression { token, expression } => {
                let expression = expression.as_ref().expect("Expression should not be None");
                if let Some(identifier) = expression.as_any().downcast_ref::<Identifier>() {
                    assert_eq!(identifier.value, "foobar".to_string());
                    assert_eq!(identifier.token_literal(), "foobar".to_string());
                } else {
                    panic!("expression must be an identifier");
                }
            }
            _ => panic!("Unexpected Statement variant"),
        }
    }

    #[test]
    fn integer_literal_expression() {
        let input = "5;";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let had_errors = check_errors(&parser);

        assert!(!had_errors);

        assert_eq!(
            program.statements.len(),
            1,
            "program should have 1 statement"
        );

        match &program.statements[0] {
            Statement::Expression {
                token: _token,
                expression,
            } => {
                let expression = expression.as_ref().expect("Expression should not be None");
                if let Some(identifier) = expression.as_any().downcast_ref::<IntegerLiteral>() {
                    assert_eq!(identifier.value, 5);
                    assert_eq!(identifier.token_literal(), "5".to_string());
                } else {
                    panic!("expression must be an identifier");
                }
            }
            _ => panic!("Unexpected Statement variant"),
        }
    }
}
