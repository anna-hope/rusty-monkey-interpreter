use std::collections::HashMap;

use lazy_static::lazy_static;
use thiserror::Error;

use crate::ast::{BlockStatement, Expression, Identifier, Program, Statement};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("Missing if condition")]
    MissingIfCondition,

    #[error("{0}")]
    Error(String),
}

pub type Result<T> = std::result::Result<T, ParserError>;

type PrefixParseFn = fn(&mut Parser) -> Result<Expression>;
type InfixParseFn = fn(&mut Parser, &Expression) -> Result<Expression>;

#[derive(Debug, PartialOrd, PartialEq, Copy, Clone)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

lazy_static! {
    static ref PRECEDENCES: HashMap<TokenType, Precedence> = {
        let mut m = HashMap::new();
        m.insert(TokenType::Eq, Precedence::Equals);
        m.insert(TokenType::NotEq, Precedence::Equals);
        m.insert(TokenType::Lt, Precedence::LessGreater);
        m.insert(TokenType::Gt, Precedence::LessGreater);
        m.insert(TokenType::Plus, Precedence::Sum);
        m.insert(TokenType::Minus, Precedence::Sum);
        m.insert(TokenType::Slash, Precedence::Product);
        m.insert(TokenType::Asterisk, Precedence::Product);
        m.insert(TokenType::Lparen, Precedence::Call);
        m
    };
}

lazy_static! {
    static ref PREFIX_PARSE_FNS: HashMap<TokenType, PrefixParseFn> = {
        let mut m: HashMap<TokenType, PrefixParseFn> = HashMap::new();
        m.insert(TokenType::Ident, Parser::parse_identifier);
        m.insert(TokenType::Int, Parser::parse_integer_literal);
        m.insert(TokenType::Bang, Parser::parse_prefix_expression);
        m.insert(TokenType::Minus, Parser::parse_prefix_expression);
        m.insert(TokenType::True, Parser::parse_boolean);
        m.insert(TokenType::False, Parser::parse_boolean);
        m.insert(TokenType::Lparen, Parser::parse_grouped_expression);
        m.insert(TokenType::If, Parser::parse_if_expression);
        m.insert(TokenType::Function, Parser::parse_function_literal);
        m
    };
}

lazy_static! {
    static ref INFIX_PARSE_FNS: HashMap<TokenType, InfixParseFn> = {
        let mut m: HashMap<TokenType, InfixParseFn> = HashMap::new();
        m.insert(TokenType::Plus, Parser::parse_infix_expression);
        m.insert(TokenType::Minus, Parser::parse_infix_expression);
        m.insert(TokenType::Slash, Parser::parse_infix_expression);
        m.insert(TokenType::Asterisk, Parser::parse_infix_expression);
        m.insert(TokenType::Eq, Parser::parse_infix_expression);
        m.insert(TokenType::NotEq, Parser::parse_infix_expression);
        m.insert(TokenType::Lt, Parser::parse_infix_expression);
        m.insert(TokenType::Gt, Parser::parse_infix_expression);
        m.insert(TokenType::Lparen, Parser::parse_call_expression);
        m
    };
}

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    pub errors: Vec<String>,
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

    fn add_error(&mut self, msg: String) {
        // Doing this as a method instead of inlining to simplify debugging.
        self.errors.push(msg);
    }

    fn expect_peek(&mut self, token_type: TokenType) -> Result<()> {
        if self.peek_token.token_type == token_type {
            self.advance();
            Ok(())
        } else {
            let msg = format!(
                "expected next token to be {}, got {} instead",
                token_type, self.peek_token.token_type
            );
            Err(ParserError::Error(msg))
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while self.current_token.token_type != TokenType::Eof {
            match self.parse_statement() {
                Ok(statement) => program.statements.push(statement),
                Err(error) => self.add_error(error.to_string()),
            }
            self.advance();
        }

        program
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        match self.current_token.token_type {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => Ok(self.parse_return_statement()),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement> {
        let token = self.current_token.clone();

        self.expect_peek(TokenType::Ident)?;

        let name = Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        };

        self.expect_peek(TokenType::Assign)?;
        self.advance();

        let value = self.parse_expression(Precedence::Lowest);

        if self.peek_token.token_type == TokenType::Semicolon {
            self.advance();
        }

        Ok(Statement::Let { token, name, value })
    }

    fn parse_return_statement(&mut self) -> Statement {
        let token = self.current_token.clone();
        self.advance();

        let value = self.parse_expression(Precedence::Lowest);
        if self.peek_token.token_type == TokenType::Semicolon {
            self.advance();
        }

        Statement::Return { token, value }
    }

    fn parse_expression_statement(&mut self) -> Result<Statement> {
        let token = self.current_token.clone();
        let expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token.token_type == TokenType::Semicolon {
            self.advance();
        }
        let statement = Statement::ExpressionStatement { token, expression };
        Ok(statement)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        if let Some(prefix) = PREFIX_PARSE_FNS.get(&self.current_token.token_type) {
            match prefix(self) {
                Ok(mut left_expression) => {
                    while self.peek_token.token_type != TokenType::Semicolon
                        && precedence < self.peek_precedence()
                    {
                        if let Some(infix) = INFIX_PARSE_FNS.get(&self.peek_token.token_type) {
                            self.advance();
                            match infix(self, &left_expression) {
                                Ok(expression) => left_expression = expression,
                                Err(error) => {
                                    self.add_error(error.to_string());
                                    return None;
                                }
                            }
                        } else {
                            return Some(left_expression);
                        }
                    }
                    Some(left_expression)
                }
                Err(error) => {
                    self.add_error(error.to_string());
                    None
                }
            }
        } else {
            let msg = format!(
                "No prefix parse function found for {}",
                self.current_token.token_type
            );
            self.add_error(msg);
            None
        }
    }

    fn parse_identifier(&mut self) -> Result<Expression> {
        Ok(Expression::Identifier(Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        }))
    }

    fn parse_integer_literal(&mut self) -> Result<Expression> {
        let token = self.current_token.clone();
        let value = token
            .literal
            .parse::<i64>()
            .map_err(|error| ParserError::Error(error.to_string()))?;
        Ok(Expression::IntegerLiteral { token, value })
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression> {
        let token = self.current_token.clone();
        let operator = self.current_token.literal.clone();

        self.advance();
        let right = self.parse_expression(Precedence::Prefix).map(Box::new);

        Ok(Expression::Prefix {
            token,
            operator,
            right,
        })
    }

    fn peek_precedence(&self) -> Precedence {
        match PRECEDENCES.get(&self.peek_token.token_type) {
            Some(precedence) => *precedence,
            None => Precedence::Lowest,
        }
    }

    fn current_precedence(&self) -> Precedence {
        match PRECEDENCES.get(&self.current_token.token_type) {
            Some(precedence) => *precedence,
            None => Precedence::Lowest,
        }
    }

    fn parse_infix_expression(&mut self, left: &Expression) -> Result<Expression> {
        let token = self.current_token.clone();
        let operator = self.current_token.literal.clone();

        let precedence = self.current_precedence();
        self.advance();
        let right = self.parse_expression(precedence).map(Box::new);

        Ok(Expression::Infix {
            token,
            operator,
            left: Some(Box::new(left.clone())),
            right,
        })
    }

    fn parse_boolean(&mut self) -> Result<Expression> {
        Ok(Expression::Boolean {
            token: self.current_token.clone(),
            value: self.current_token.token_type == TokenType::True,
        })
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression> {
        self.advance();
        let expression = self.parse_expression(Precedence::Lowest).ok_or_else(|| {
            ParserError::Error("Could not parse the nested expression".to_string())
        })?;

        self.expect_peek(TokenType::Rparen)?;
        Ok(expression)
    }

    fn parse_if_expression(&mut self) -> Result<Expression> {
        let token = self.current_token.clone();

        self.expect_peek(TokenType::Lparen)?;

        self.advance();
        let condition = self
            .parse_expression(Precedence::Lowest)
            .map(Box::new)
            .ok_or(ParserError::MissingIfCondition)?;

        self.expect_peek(TokenType::Rparen)?;
        self.expect_peek(TokenType::Lbrace)?;

        let consequence = self.parse_block_statement();

        let alternative = if self.peek_token.token_type == TokenType::Else {
            self.advance();
            self.expect_peek(TokenType::Lbrace)?;
            Some(self.parse_block_statement())
        } else {
            None
        };

        Ok(Expression::If {
            token,
            condition,
            alternative,
            consequence,
        })
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let token = self.current_token.clone();
        let mut statements = vec![];

        self.advance();

        while self.current_token.token_type != TokenType::Rbrace
            && self.current_token.token_type != TokenType::Eof
        {
            if let Ok(statement) = self.parse_statement() {
                statements.push(statement);
            }
            self.advance();
        }

        BlockStatement::new(token, statements)
    }

    fn parse_function_literal(&mut self) -> Result<Expression> {
        let token = self.current_token.clone();
        self.expect_peek(TokenType::Lparen)?;
        let parameters = self.parse_function_parameters()?;
        self.expect_peek(TokenType::Lbrace)?;
        let body = self.parse_block_statement();

        Ok(Expression::FunctionLiteral {
            token,
            parameters,
            body,
        })
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>> {
        let mut identifiers = vec![];

        if self.peek_token.token_type == TokenType::Rparen {
            self.advance();
            return Ok(identifiers);
        }

        self.advance();
        let identifier = Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        };
        identifiers.push(identifier);

        while self.peek_token.token_type == TokenType::Comma {
            self.advance();
            self.advance();

            let identifier = Identifier {
                token: self.current_token.clone(),
                value: self.current_token.literal.clone(),
            };
            identifiers.push(identifier);
        }

        self.expect_peek(TokenType::Rparen)?;
        Ok(identifiers)
    }

    fn parse_call_expression(&mut self, function: &Expression) -> Result<Expression> {
        let token = self.current_token.clone();
        let function = Box::new(function.to_owned());
        let arguments = self.parse_call_arguments()?;
        Ok(Expression::Call {
            token,
            function,
            arguments,
        })
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>> {
        let mut args = vec![];

        if self.peek_token.token_type == TokenType::Rparen {
            self.advance();
            return Ok(args);
        }

        self.advance();
        let expression = self
            .parse_expression(Precedence::Lowest)
            .ok_or_else(|| ParserError::Error("Could not parse expression".to_string()))?;
        args.push(expression);

        while self.peek_token.token_type == TokenType::Comma {
            self.advance();
            self.advance();
            let expression = self
                .parse_expression(Precedence::Lowest)
                .ok_or_else(|| ParserError::Error("Could not parse expression".to_string()))?;
            args.push(expression);
        }

        self.expect_peek(TokenType::Rparen)?;
        Ok(args)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Node;

    use std::any::{Any, TypeId};

    fn test_integer_literal(integer_literal: &Expression, expected_value: i64) {
        match integer_literal {
            Expression::IntegerLiteral { value, .. } => {
                assert_eq!(*value, expected_value);
                assert_eq!(integer_literal.token_literal(), expected_value.to_string());
            }
            _ => panic!("Expression must be an IntegerLiteral"),
        }
    }

    fn test_let_statement(statement: &Statement, expected_name: &str) {
        assert_eq!(statement.token_literal(), "let");
        match statement {
            Statement::Let { name, .. } => {
                assert_eq!(name.value, expected_name);
                assert_eq!(name.token.literal, expected_name);
            }
            _ => panic!("Expected Statement::Let, got {statement:?}"),
        }
    }

    fn test_identifier(expression: &Expression, value: &str) {
        match expression {
            Expression::Identifier(identifier) => {
                assert_eq!(identifier.value, value);
                assert_eq!(expression.token_literal(), value)
            }
            _ => panic!("Not an identifier: {:?}", expression),
        }
    }

    fn test_literal_expression(expression: &Expression, expected: &dyn Any) {
        let type_id = expected.type_id();
        if type_id == TypeId::of::<i64>() {
            let expected = *expected.downcast_ref::<i64>().unwrap();
            test_integer_literal(expression, expected);
        } else if type_id == TypeId::of::<i32>() {
            let expected = i64::from(*expected.downcast_ref::<i32>().unwrap());
            test_integer_literal(expression, expected);
        } else if type_id == TypeId::of::<String>() {
            let expected = expected.downcast_ref::<String>().unwrap();
            test_identifier(expression, expected);
        } else if type_id == TypeId::of::<&str>() {
            let expected = expected.downcast_ref::<&str>().unwrap();
            test_identifier(expression, expected);
        } else if type_id == TypeId::of::<bool>() {
            let expected = *expected.downcast_ref::<bool>().unwrap();
            test_boolean_literal(expression, expected);
        } else {
            panic!("'expected' has an unhandled type");
        }
    }

    fn test_boolean_literal(expression: &Expression, expected: bool) {
        match expression {
            Expression::Boolean { value, .. } => {
                assert_eq!(*value, expected);
                assert_eq!(expression.token_literal(), expected.to_string());
            }
            _ => panic!("Expression must be Boolean, got {expression:?}"),
        }
    }

    fn test_infix_expression(
        expression: &Expression,
        expected_left: &dyn Any,
        expected_operator: &str,
        expected_right: &dyn Any,
    ) {
        match expression {
            Expression::Infix {
                operator,
                left,
                right,
                ..
            } => {
                let left = left.as_ref().unwrap();
                let right = right.as_ref().unwrap();
                test_literal_expression(left.as_ref(), expected_left);
                assert_eq!(operator, expected_operator);
                test_literal_expression(right.as_ref(), expected_right);
            }
            _ => panic!("Unexpected expression variant: {:?}", expression),
        }
    }

    #[test]
    fn let_statements() {
        let tests: [(&str, &str, &dyn Any); 3] = [
            ("let x = 5;", "x", &5),
            ("let y = true;", "y", &true),
            ("let foobar = y;", "foobar", &"y"),
        ];

        for (input, expected_identifier, expected_value) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            let had_errors = has_errors(&parser);
            assert!(!had_errors);

            assert_eq!(program.statements.len(), 1);

            test_let_statement(&program.statements[0], expected_identifier);

            match &program.statements[0] {
                Statement::Let { value, .. } => {
                    let value = value.as_ref().expect("value must not be None");
                    test_literal_expression(value, expected_value);
                }
                _ => panic!("Expected Statement::Let, got {:?}", program.statements[0]),
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
        let had_errors = has_errors(&parser);
        assert!(had_errors);
        assert_eq!(parser.errors.len(), 4);
    }

    fn has_errors(parser: &Parser) -> bool {
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
        let tests: [(&str, &dyn Any); 3] = [
            ("return 5;", &5),
            ("return true;", &true),
            ("return foobar;", &"foobar"),
        ];

        for (input, expected_value) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            let had_errors = has_errors(&parser);

            assert!(!had_errors);

            assert_eq!(program.statements.len(), 1,);

            let statement = &program.statements[0];
            match statement {
                Statement::Return { value, .. } => {
                    assert_eq!(statement.token_literal(), "return".to_string());
                    let value = value.as_ref().expect("value should not be None");
                    test_literal_expression(value, expected_value);
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
        let had_errors = has_errors(&parser);

        assert!(!had_errors);

        assert_eq!(
            program.statements.len(),
            1,
            "program should have 1 statement"
        );

        match &program.statements[0] {
            Statement::ExpressionStatement { expression, .. } => {
                let expression = expression.as_ref().expect("Expression should not be None");
                match expression {
                    Expression::Identifier(identifier) => {
                        assert_eq!(identifier.value, "foobar");
                        assert_eq!(expression.token_literal(), "foobar".to_string());
                    }
                    _ => panic!("expression must be an identifier"),
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
        let had_errors = has_errors(&parser);

        assert!(!had_errors);

        assert_eq!(
            program.statements.len(),
            1,
            "program should have 1 statement"
        );

        match &program.statements[0] {
            Statement::ExpressionStatement { expression, .. } => {
                let expression = expression.as_ref().expect("Expression should not be None");
                match expression {
                    Expression::IntegerLiteral { value, .. } => {
                        assert_eq!(*value, 5);
                        assert_eq!(expression.token_literal(), "5".to_string());
                    }
                    _ => panic!("expression must be an identifier"),
                }
            }
            _ => panic!("Unexpected Statement variant"),
        }
    }

    #[test]
    fn parse_prefix_expressions() {
        let prefix_tests: &[(&str, &str, &dyn Any)] = &[
            ("!5;", "!", &5),
            ("-15;", "-", &15),
            ("!true", "!", &true),
            ("!false", "!", &false),
        ];

        for (input, expected_operator, expected_value) in prefix_tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            assert!(!has_errors(&parser));

            assert_eq!(
                program.statements.len(),
                1,
                "Expected program to have {} statements, got {}",
                1,
                program.statements.len()
            );

            match &program.statements[0] {
                Statement::ExpressionStatement { expression, .. } => {
                    let expression = expression.as_ref().expect("Expression should not be None");
                    match expression {
                        Expression::Prefix {
                            operator, right, ..
                        } => {
                            assert_eq!(operator, expected_operator);
                            let statement_right = right.as_ref().unwrap();
                            test_literal_expression(statement_right.as_ref(), *expected_value);
                        }
                        _ => panic!("statement is not a PrefixExpression."),
                    }
                }
                _ => panic!("Unexpected statement: {:?}", program.statements[0]),
            }
        }
    }

    #[test]
    fn parse_infix_expressions() {
        let infix_tests: &[(&str, &dyn Any, &str, &dyn Any)] = &[
            ("5 + 5", &5, "+", &5),
            ("5 - 5", &5, "-", &5),
            ("5 * 5", &5, "*", &5),
            ("5 / 5", &5, "/", &5),
            ("5 > 5", &5, ">", &5),
            ("5 < 5", &5, "<", &5),
            ("5 == 5", &5, "==", &5),
            ("5 != 5", &5, "!=", &5),
            ("true == true", &true, "==", &true),
            ("true != false", &true, "!=", &false),
            ("false == false", &false, "==", &false),
        ];

        for (input, left_value, expected_operator, right_value) in infix_tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            assert!(!has_errors(&parser));

            assert_eq!(
                program.statements.len(),
                1,
                "expected program to have {} statements, got {}",
                1,
                program.statements.len()
            );

            match &program.statements[0] {
                Statement::ExpressionStatement {
                    token: _token,
                    expression,
                } => {
                    let expression = expression.as_ref().unwrap();
                    match expression {
                        Expression::Infix {
                            left,
                            right,
                            operator,
                            ..
                        } => {
                            let statement_left = left.as_ref().unwrap().as_ref();
                            test_literal_expression(statement_left, *left_value);

                            assert_eq!(operator, expected_operator);

                            let statement_right = right.as_ref().unwrap().as_ref();
                            test_literal_expression(statement_right, *right_value);
                        }
                        _ => panic!("Unexpected expression type"),
                    }
                }
                _ => panic!("Unexpected Statement: {:?}", program.statements[0]),
            }
        }
    }

    #[test]
    fn parse_operator_precedence() {
        let tests = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            assert!(!has_errors(&parser));

            let actual = program.to_string();
            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn boolean_expression() {
        let tests = [("true;", true), ("false;", false)];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            assert!(!has_errors(&parser));

            assert_eq!(
                program.statements.len(),
                1,
                "Expected program to have {} statements, got {}",
                1,
                program.statements.len()
            );

            match &program.statements[0] {
                Statement::ExpressionStatement { expression, .. } => {
                    let expression = expression.as_ref().unwrap();
                    match expression {
                        Expression::Boolean { value, .. } => {
                            assert_eq!(*value, expected);
                        }
                        _ => panic!("Unexpected Expression: {:?}", expression),
                    }
                }
                _ => panic!("Unexpected Statement: {:?}", program.statements[0]),
            }
        }
    }

    #[test]
    fn if_expression() {
        let input = "if (x < y) { x }";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert!(!has_errors(&parser));

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::ExpressionStatement { expression, .. } => {
                let expression = expression.as_ref().expect("Expression must not be None");
                match expression {
                    Expression::If {
                        condition,
                        consequence,
                        alternative,
                        ..
                    } => {
                        test_infix_expression(condition, &"x", "<", &"y");
                        assert_eq!(consequence.statements.len(), 1);
                        match &consequence.statements[0] {
                            Statement::ExpressionStatement { expression, .. } => {
                                let expression =
                                    expression.as_ref().expect("Expression must not be None");
                                test_identifier(expression, "x");
                            }
                            _ => panic!(
                                "Must be an ExpressionStatement, got {:?}",
                                consequence.statements[0]
                            ),
                        }
                        assert!(alternative.is_none());
                    }
                    _ => panic!("Must be Expression::If, got {expression:?}"),
                }
            }
            _ => panic!(
                "Must be an ExpressionStatement, got {:?}",
                program.statements[0]
            ),
        }
    }

    #[test]
    fn if_else_expression() {
        let input = "if (x < y) { x } else { y }";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert!(!has_errors(&parser));

        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn parse_function_literal() {
        let input = "fn (x, y) { x + y; }";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert!(!has_errors(&parser));

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::ExpressionStatement { expression, .. } => {
                let expression = expression.as_ref().expect("expression should not be None");
                match expression {
                    Expression::FunctionLiteral {
                        parameters, body, ..
                    } => {
                        assert_eq!(parameters.len(), 2);

                        let expected_parameters = ["x", "y"];
                        for (parameter, expected) in parameters.iter().zip(expected_parameters) {
                            let parameter_expression = Expression::Identifier(parameter.to_owned());
                            test_literal_expression(&parameter_expression, &expected);
                        }

                        assert_eq!(body.statements.len(), 1);

                        match &body.statements[0] {
                            Statement::ExpressionStatement { expression, .. } => {
                                let expression =
                                    expression.as_ref().expect("expression should not be None");
                                test_infix_expression(expression, &"x", "+", &"y");
                            }
                            _ => panic!("Function body must be ExpressionStatement"),
                        }
                    }
                    _ => panic!("Must be FunctionLiteral, got {expression:?}"),
                }
            }
            _ => panic!(
                "Expected ExpressionStatement, got {:?}",
                program.statements[0]
            ),
        }
    }

    #[test]
    fn parse_function_parameters() {
        let tests = [
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x".to_string()]),
            (
                "fn(x, y, z) {};",
                vec!["x".to_string(), "y".to_string(), "z".to_string()],
            ),
        ];

        for (input, expected_params) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            assert!(!has_errors(&parser));

            match &program.statements[0] {
                Statement::ExpressionStatement { expression, .. } => {
                    let expression = expression.as_ref().expect("expression should not be None");
                    match expression {
                        Expression::FunctionLiteral { parameters, .. } => {
                            assert_eq!(parameters.len(), expected_params.len());

                            for (param, expected_param) in parameters.iter().zip(expected_params) {
                                let param_expression = Expression::Identifier(param.to_owned());
                                test_literal_expression(&param_expression, &expected_param)
                            }
                        }
                        _ => panic!("Must be a FunctionLiteral"),
                    }
                }
                _ => panic!("Must be an ExpressionStatement"),
            }
        }
    }

    #[test]
    fn parse_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert!(!has_errors(&parser));

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::ExpressionStatement { expression, .. } => {
                let expression = expression.as_ref().expect("Expression should not be None");
                match expression {
                    Expression::Call {
                        function,
                        arguments,
                        ..
                    } => {
                        test_identifier(function, "add");
                        assert_eq!(arguments.len(), 3);

                        test_literal_expression(&arguments[0], &1);
                        test_infix_expression(&arguments[1], &2, "*", &3);
                        test_infix_expression(&arguments[2], &4, "+", &5);
                    }
                    _ => panic!("Must be an Expression::Call, got {expression:?}"),
                }
            }
            _ => panic!(
                "Should be an ExpressionStatement, got {:?}",
                program.statements[0]
            ),
        }
    }
}
