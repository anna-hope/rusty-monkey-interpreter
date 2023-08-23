use crate::token::{Token, TokenType};

pub trait Node {
    fn token_literal(&self) -> String;
}

pub trait Expression: Node {
    fn expression(&self);
}

pub enum Statement {
    Let {
        token: Token,
        name: Identifier,
        value: Option<Box<dyn Expression>>, // Temporarily make it an Option.
    },
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::Let { token, .. } => token.literal.to_owned(),
        }
    }
}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Identifier {
    pub fn new(token: Token, value: String) -> Self {
        Self { token, value }
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.to_owned()
    }
}

impl Expression for Identifier {
    fn expression(&self) {}
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

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.is_empty() {
            "".to_string()
        } else {
            self.statements[0].token_literal()
        }
    }
}
