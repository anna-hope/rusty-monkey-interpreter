use lazy_static::lazy_static;
use std::collections::HashMap;
use std::fmt::Formatter;

lazy_static! {
    static ref KEYWORDS: HashMap<String, TokenType> = {
        let mut m = HashMap::new();
        m.insert("fn".to_string(), TokenType::Function);
        m.insert("let".to_string(), TokenType::Let);
        m
    };
}

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum TokenType {
    Illegal,
    Eof,

    // Identifiers + literals
    Ident,
    Int,

    // Operators
    Assign,
    Plus,

    // Delimiters
    Comma,
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    // Keywords
    Function,
    Let,
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            TokenType::Illegal => "ILLEGAL",
            TokenType::Eof => "EOF",
            TokenType::Ident => "IDENT",
            TokenType::Int => "INT",
            TokenType::Assign => "=",
            TokenType::Plus => "+",
            TokenType::Comma => ",",
            TokenType::Semicolon => ";",
            TokenType::Lparen => "(",
            TokenType::Rparen => ")",
            TokenType::Lbrace => "{",
            TokenType::Rbrace => "}",
            TokenType::Function => "FUNCTION",
            TokenType::Let => "LET",
        }
        .to_string();
        write!(f, "{string}")
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, character: u8) -> Self {
        Self {
            token_type,
            literal: (character as char).to_string(),
        }
    }

    /// Constructs a new Token with token_type Ident and the given String literal.
    pub fn new_with_literal(token_type: TokenType, literal: String) -> Self {
        Self {
            token_type,
            literal,
        }
    }
}

pub fn lookup_ident(ident: &str) -> TokenType {
    if let Some(token_type) = KEYWORDS.get(ident) {
        token_type.to_owned()
    } else {
        TokenType::Ident
    }
}
