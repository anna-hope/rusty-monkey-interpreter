use std::any::Any;
use std::fmt::{Debug, Display, Formatter};

use crate::token::Token;

pub trait Node: Display {
    fn token_literal(&self) -> String;
}

impl Debug for dyn Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Node")
            .field("token_literal", &self.token_literal())
            .finish()
    }
}

pub trait Expression: Node {
    fn expression(&self);

    /// This is needed primarily for testing that the expression
    /// was parsed into the expected type.
    fn as_any(&self) -> &dyn Any;
}

impl Debug for dyn Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Expression")
            .field("token_literal", &self.token_literal())
            .finish()
    }
}

#[derive(Debug)]
pub enum Statement {
    Let {
        token: Token,
        name: Identifier,
        value: Option<Box<dyn Expression>>, // Temporarily make it an Option while this is WIP.
    },
    Return {
        token: Token,
        value: Option<Box<dyn Expression>>,
    },
    Expression {
        token: Token,
        expression: Option<Box<dyn Expression>>,
    },
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Statement::Let { token, name, value } => {
                let mut string = String::new();
                string.push_str(format!("{} {} = ", token.literal, name.to_string()).as_str());
                if let Some(value) = value {
                    string.push_str(format!("{}", value.to_string()).as_str());
                }
                string.push(';');
                string
            }
            Statement::Return { token, value } => {
                let mut string = String::new();
                string.push_str(format!("{} ", token.literal).as_str());
                if let Some(value) = value {
                    string.push_str(format!("{}", value.to_string()).as_str());
                }
                string.push(';');
                string
            }
            Statement::Expression { token, expression } => {
                if let Some(expression) = expression {
                    expression.to_string()
                } else {
                    "".to_string()
                }
            }
        };
        write!(f, "{string}")
    }
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::Let { token, .. } => token.literal.to_owned(),
            Statement::Return { token, .. } => token.literal.to_owned(),
            Statement::Expression { token, .. } => token.literal.to_owned(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Identifier {
    pub fn new(token: Token, value: String) -> Self {
        Self { token, value }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.to_owned()
    }
}

impl Expression for Identifier {
    fn expression(&self) {}

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl IntegerLiteral {
    pub fn new(token: Token, value: i64) -> Self {
        Self { token, value }
    }
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.to_owned()
    }
}

impl Expression for IntegerLiteral {
    fn expression(&self) {}

    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Self { statements: vec![] }
    }
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut string = String::new();
        for statement in self.statements.iter() {
            string.push_str(statement.to_string().as_str());
        }
        write!(f, "{string}")
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.is_empty() {
            "".to_string()
        } else {
            self.statements[0].token_literal()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenType;

    #[test]
    fn program_string() {
        let program = Program {
            statements: vec![Statement::Let {
                token: Token::new(TokenType::Let, "let".to_string()),
                name: Identifier::new(
                    Token::new(TokenType::Ident, "myVar".to_string()),
                    "myVar".to_string(),
                ),
                value: Some(Box::new(Identifier::new(
                    Token::new(TokenType::Ident, "anotherVar".to_string()),
                    "anotherVar".to_string(),
                ))),
            }],
        };

        assert_eq!(program.to_string(), "let myVar = anotherVar;")
    }
}
