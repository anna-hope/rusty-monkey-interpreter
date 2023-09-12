use std::collections::HashMap;
use std::fmt::Formatter;

use lazy_static::lazy_static;

lazy_static! {
    static ref KEYWORDS: HashMap<String, TokenType> = {
        let mut m = HashMap::new();
        m.insert("fn".to_string(), TokenType::Function);
        m.insert("let".to_string(), TokenType::Let);
        m.insert("true".to_string(), TokenType::True);
        m.insert("false".to_string(), TokenType::False);
        m.insert("if".to_string(), TokenType::If);
        m.insert("else".to_string(), TokenType::Else);
        m.insert("return".to_string(), TokenType::Return);
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
    Minus,
    Bang,
    Asterisk,
    Slash,

    Lt,
    Gt,

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
    True,
    False,
    If,
    Else,
    Return,

    Eq,
    NotEq,
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
            TokenType::Minus => "/",
            TokenType::Bang => "!",
            TokenType::Asterisk => "*",
            TokenType::Slash => "/",
            TokenType::Lt => "<",
            TokenType::Gt => ">",
            TokenType::Comma => ",",
            TokenType::Semicolon => ";",
            TokenType::Lparen => "(",
            TokenType::Rparen => ")",
            TokenType::Lbrace => "{",
            TokenType::Rbrace => "}",
            TokenType::Function => "FUNCTION",
            TokenType::Let => "LET",
            TokenType::True => "TRUE",
            TokenType::False => "FALSE",
            TokenType::If => "IF",
            TokenType::Else => "ELSE",
            TokenType::Return => "RETURN",
            TokenType::Eq => "==",
            TokenType::NotEq => "!=",
        }
        .to_string();
        write!(f, "{string}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new_with_character(token_type: TokenType, character: u8) -> Self {
        Self {
            token_type,
            literal: (char::from(character)).to_string(),
        }
    }

    /// Constructs a new Token with token_type Ident and the given String literal.
    pub fn new(token_type: TokenType, literal: String) -> Self {
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
