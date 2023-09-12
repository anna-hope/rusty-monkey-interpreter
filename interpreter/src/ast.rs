use std::fmt::{Debug, Display, Formatter};

use crate::token::Token;

pub trait Node: Display {
    fn token_literal(&self) -> String;
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral {
        token: Token,
        value: i64,
    },
    Prefix {
        token: Token,
        operator: String,
        right: Box<Expression>,
    },
    Infix {
        token: Token,
        operator: String,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Boolean {
        token: Token,
        value: bool,
    },
    If {
        token: Token,
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    },
    FunctionLiteral {
        token: Token, // the 'fn' token
        parameters: Vec<Identifier>,
        body: BlockStatement,
    },
    Call {
        token: Token, // The '(' token
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Self::Identifier(identifier) => identifier.value.to_owned(),
            Self::IntegerLiteral { value, .. } => value.to_string(),
            Self::Prefix {
                operator, right, ..
            } => format!("({operator}{right})"),

            Self::Infix {
                operator,
                left,
                right,
                ..
            } => format!("({left} {operator} {right})"),
            Self::Boolean { token, .. } => token.literal.clone(),
            Self::If {
                condition,
                consequence,
                alternative,
                ..
            } => {
                let mut string = format!("if {condition} {consequence}");
                if let Some(alternative) = alternative {
                    string.push_str("else ");
                    string.push_str(alternative.to_string().as_str());
                }
                string
            }
            Self::FunctionLiteral {
                token,
                parameters,
                body,
            } => {
                let params = parameters
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                let string = format!("{} ({params}) {body}", token.literal);
                string
            }
            Self::Call {
                function,
                arguments,
                ..
            } => {
                let args = arguments
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{function}({args})")
            }
        };
        write!(f, "{string}")
    }
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        let token = match self {
            Self::Identifier(identifier) => &identifier.token,
            Self::IntegerLiteral { token, .. } => token,
            Self::Prefix { token, .. } => token,
            Self::Infix { token, .. } => token,
            Self::Boolean { token, .. } => token,
            Self::If { token, .. } => token,
            Self::FunctionLiteral { token, .. } => token,
            Self::Call { token, .. } => token,
        };
        token.literal.clone()
    }
}

impl Expression {
    fn expression(&self) {}
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement {
    token: Token,
    pub statements: Vec<Statement>,
}

impl BlockStatement {
    pub fn new(token: Token, statements: Vec<Statement>) -> Self {
        Self { token, statements }
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut string = String::new();
        for statement in self.statements.iter() {
            string.push_str(statement.to_string().as_str());
        }
        write!(f, "{string}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let {
        token: Token,
        name: Identifier,
        value: Expression,
    },
    Return {
        token: Token,
        value: Expression,
    },
    ExpressionStatement {
        token: Token,
        expression: Expression,
    },
    Block(BlockStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Self::Let { token, name, value } => format!("{} {name} = {value};", token.literal),
            Self::Return { token, value } => format!("{} {value};", token.literal),
            Self::ExpressionStatement { expression, .. } => expression.to_string(),
            Self::Block(block_statement) => block_statement.to_string(),
        };
        write!(f, "{string}")
    }
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Self::Let { token, .. } => token.literal.to_owned(),
            Self::Return { token, .. } => token.literal.to_owned(),
            Self::ExpressionStatement { token, .. } => token.literal.to_owned(),
            Self::Block(block_statement) => block_statement.token.literal.to_owned(),
        }
    }
}

#[derive(Debug)]
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
                name: Identifier {
                    token: Token::new(TokenType::Ident, "myVar".to_string()),
                    value: "myVar".to_string(),
                },
                value: Expression::Identifier(Identifier {
                    token: Token::new(TokenType::Ident, "anotherVar".to_string()),
                    value: "anotherVar".to_string(),
                }),
            }],
        };

        assert_eq!(program.to_string(), "let myVar = anotherVar;")
    }
}
