use super::token::{Token,TokenType};

struct Lexer<'a> {
    input: &'a String,
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    ch: char,
    eof: bool,
}

impl<'a> Lexer<'a> {
    fn new(input: &String) -> Lexer {
        let mut l = Lexer{input, chars: input.chars().peekable(), ch: '_', eof: false};
        l.read_char();
        l
    }

    fn next_token(&mut self) -> Token {
        let tok = if self.eof {
            Lexer::new_token(TokenType::Eof, String::from(""))
        } else {
            match self.ch {
                '=' => Lexer::new_token(TokenType::Assign, self.ch.to_string()),
                ';' => Lexer::new_token(TokenType::Semicolon, self.ch.to_string()),
                '(' => Lexer::new_token(TokenType::Lparen, self.ch.to_string()),
                ')' => Lexer::new_token(TokenType::Rparen, self.ch.to_string()),
                ',' => Lexer::new_token(TokenType::Comma, self.ch.to_string()),
                '+' => Lexer::new_token(TokenType::Plus, self.ch.to_string()),
                '{' => Lexer::new_token(TokenType::Lbrace, self.ch.to_string()),
                '}' => Lexer::new_token(TokenType::Rbrace, self.ch.to_string()),
                _ => Lexer::new_token( TokenType::Illegal, self.ch.to_string())
            }
        };

        self.read_char();
        tok
    }

    fn read_char(&mut self) {
        match self.chars.next() {
            Some(ch) => self.ch = ch,
            None => self.eof = true
        }
    }

    fn new_token(t: TokenType, literal: String) -> Token {
        Token{t, literal}
    }
}

#[cfg(test)]
mod tests {
    use super::TokenType;
    use super::Lexer;

    #[test]
    fn test_next_token() {
        let input = "=+(){},;".to_string();

        struct Test<'a> {
            expected_type: TokenType,
            expected_literal: &'a str
        }

        let tests = [
            Test {expected_type: TokenType::Assign,    expected_literal: "="},
            Test {expected_type: TokenType::Plus,      expected_literal: "+"},
            Test {expected_type: TokenType::Lparen,    expected_literal: "("},
            Test {expected_type: TokenType::Rparen,    expected_literal: ")"},
            Test {expected_type: TokenType::Lbrace,    expected_literal: "{"},
            Test {expected_type: TokenType::Rbrace,    expected_literal: "}"},
            Test {expected_type: TokenType::Comma,     expected_literal: ","},
            Test {expected_type: TokenType::Semicolon, expected_literal: ";"},
            Test {expected_type: TokenType::Eof,       expected_literal: ""},
        ];

        let mut l = Lexer::new(&input);

        for tt in &tests {
            let tok = l.next_token();
            assert_eq!(tok.t, tt.expected_type);
            assert_eq!(tok.literal, tt.expected_literal);
        }
    }
}

