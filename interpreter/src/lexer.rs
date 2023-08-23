use crate::token::{lookup_ident, Token, TokenType};

#[derive(Debug)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    character: u8,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            input,
            position: 0,
            read_position: 0,
            character: 0,
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.character = 0;
        } else {
            self.character = self.input.as_bytes()[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.character.into() {
            '=' => {
                if self.peek() as char == '=' {
                    let character = self.character as char;
                    self.read_char();
                    Token::new_with_literal(
                        TokenType::Eq,
                        format!("{character}{}", self.character as char),
                    )
                } else {
                    Token::new(TokenType::Assign, self.character)
                }
            }
            '+' => Token::new(TokenType::Plus, self.character),
            '-' => Token::new(TokenType::Minus, self.character),
            '!' => {
                if self.peek() as char == '=' {
                    let character = self.character as char;
                    self.read_char();
                    Token::new_with_literal(
                        TokenType::NotEq,
                        format!("{character}{}", self.character as char),
                    )
                } else {
                    Token::new(TokenType::Bang, self.character)
                }
            }
            '/' => Token::new(TokenType::Slash, self.character),
            '*' => Token::new(TokenType::Asterisk, self.character),
            '<' => Token::new(TokenType::Lt, self.character),
            '>' => Token::new(TokenType::Gt, self.character),
            ';' => Token::new(TokenType::Semicolon, self.character),
            '(' => Token::new(TokenType::Lparen, self.character),
            ')' => Token::new(TokenType::Rparen, self.character),
            ',' => Token::new(TokenType::Comma, self.character),
            '{' => Token::new(TokenType::Lbrace, self.character),
            '}' => Token::new(TokenType::Rbrace, self.character),
            '\0' => Token::new(TokenType::Eof, 0),
            _ => {
                if is_letter(self.character as char) {
                    let literal = self.read_identifier();
                    let token_type = lookup_ident(&literal);
                    return Token::new_with_literal(token_type, literal);
                } else if self.character.is_ascii_digit() {
                    let token_type = TokenType::Int;
                    let literal = self.read_number();
                    return Token::new_with_literal(token_type, literal);
                } else {
                    Token::new(TokenType::Illegal, self.character)
                }
            }
        };
        self.read_char();
        token
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while is_letter(self.character.into()) {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while self.character.is_ascii_digit() {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn skip_whitespace(&mut self) {
        while self.character.is_ascii_whitespace() {
            self.read_char()
        }
    }

    fn peek(&self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input.as_bytes()[self.read_position]
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();
        if token.token_type != TokenType::Eof {
            Some(token)
        } else {
            None
        }
    }
}

fn is_letter(character: char) -> bool {
    character.is_ascii_alphabetic() || character == '_'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn next_token() {
        let input = r#"let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y;
}

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10; 
10 != 9;
"#;
        let mut lexer = Lexer::new(input.to_string());

        let tests = [
            (TokenType::Let, "let"),
            (TokenType::Ident, "five"),
            (TokenType::Assign, "="),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "ten"),
            (TokenType::Assign, "="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "add"),
            (TokenType::Assign, "="),
            (TokenType::Function, "fn"),
            (TokenType::Lparen, "("),
            (TokenType::Ident, "x"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "y"),
            (TokenType::Rparen, ")"),
            (TokenType::Lbrace, "{"),
            (TokenType::Ident, "x"),
            (TokenType::Plus, "+"),
            (TokenType::Ident, "y"),
            (TokenType::Semicolon, ";"),
            (TokenType::Rbrace, "}"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "result"),
            (TokenType::Assign, "="),
            (TokenType::Ident, "add"),
            (TokenType::Lparen, "("),
            (TokenType::Ident, "five"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "ten"),
            (TokenType::Rparen, ")"),
            (TokenType::Semicolon, ";"),
            (TokenType::Bang, "!"),
            (TokenType::Minus, "-"),
            (TokenType::Slash, "/"),
            (TokenType::Asterisk, "*"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "5"),
            (TokenType::Lt, "<"),
            (TokenType::Int, "10"),
            (TokenType::Gt, ">"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::If, "if"),
            (TokenType::Lparen, "("),
            (TokenType::Int, "5"),
            (TokenType::Lt, "<"),
            (TokenType::Int, "10"),
            (TokenType::Rparen, ")"),
            (TokenType::Lbrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::True, "true"),
            (TokenType::Semicolon, ";"),
            (TokenType::Rbrace, "}"),
            (TokenType::Else, "else"),
            (TokenType::Lbrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::False, "false"),
            (TokenType::Semicolon, ";"),
            (TokenType::Rbrace, "}"),
            (TokenType::Int, "10"),
            (TokenType::Eq, "=="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "10"),
            (TokenType::NotEq, "!="),
            (TokenType::Int, "9"),
            (TokenType::Semicolon, ";"),
            (TokenType::Eof, "\0"),
        ];

        for (index, (expected_type, expected_literal)) in tests.into_iter().enumerate() {
            let token = lexer.next_token();
            assert_eq!(
                token.token_type, expected_type,
                "testing token {index}: {token:?}"
            );
            assert_eq!(
                token.literal,
                expected_literal.to_string(),
                "testing token {index}: {token:?}"
            );
        }
    }
}
