use super::token::{lookup_ident, Token, TokenType};

pub struct Lexer {
    chars: std::iter::Peekable<std::vec::IntoIter<char>>,
    ch: Option<char>,
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        let mut l = Lexer {
            chars: input.chars().collect::<Vec<_>>().into_iter().peekable(),
            ch: None,
        };
        l.read_char();
        l
    }

    fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        let tok = if let Some(ch) = self.ch {
            Some(match ch {
                '=' => {
                    if self.peek_char().is_some() && self.peek_char().unwrap() == &'=' {
                        self.read_char();
                        let literal: String = [ch, self.ch.unwrap()].iter().collect();
                        Token {
                            t: TokenType::Eq,
                            literal,
                        }
                    } else {
                        Token {
                            t: TokenType::Assign,
                            literal: ch.to_string(),
                        }
                    }
                }
                '+' => Token {
                    t: TokenType::Plus,
                    literal: ch.to_string(),
                },
                '-' => Token {
                    t: TokenType::Minus,
                    literal: ch.to_string(),
                },
                '!' => {
                    if self.peek_char().is_some() && self.peek_char().unwrap() == &'=' {
                        self.read_char();
                        let literal: String = [ch, self.ch.unwrap()].iter().collect();
                        Token {
                            t: TokenType::NotEq,
                            literal,
                        }
                    } else {
                        Token {
                            t: TokenType::Bang,
                            literal: ch.to_string(),
                        }
                    }
                }
                '*' => Token {
                    t: TokenType::Asterisk,
                    literal: ch.to_string(),
                },
                '/' => Token {
                    t: TokenType::Slash,
                    literal: ch.to_string(),
                },
                '<' => Token {
                    t: TokenType::Lt,
                    literal: ch.to_string(),
                },
                '>' => Token {
                    t: TokenType::Gt,
                    literal: ch.to_string(),
                },
                ',' => Token {
                    t: TokenType::Comma,
                    literal: ch.to_string(),
                },
                ';' => Token {
                    t: TokenType::Semicolon,
                    literal: ch.to_string(),
                },
                '(' => Token {
                    t: TokenType::LParen,
                    literal: ch.to_string(),
                },
                ')' => Token {
                    t: TokenType::RParen,
                    literal: ch.to_string(),
                },
                '{' => Token {
                    t: TokenType::LBrace,
                    literal: ch.to_string(),
                },
                '}' => Token {
                    t: TokenType::RBrace,
                    literal: ch.to_string(),
                },
                '"' => Token {
                    t: TokenType::String,
                    literal: self.read_string(),
                },
                '[' => Token {
                    t: TokenType::LBracket,
                    literal: ch.to_string(),
                },
                ']' => Token {
                    t: TokenType::RBracket,
                    literal: ch.to_string(),
                },
                ':' => Token {
                    t: TokenType::Colon,
                    literal: ch.to_string(),
                },
                'a'..='z' | 'A'..='Z' | '_' => return Some(self.read_identifier()),
                '0'..='9' => return Some(self.read_number()),
                _ => Token {
                    t: TokenType::Illegal,
                    literal: ch.to_string(),
                },
            })
        } else {
            None
        };

        self.read_char();
        tok
    }

    fn read_char(&mut self) {
        self.ch = self.chars.next();
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn read_identifier(&mut self) -> Token {
        let mut literal = String::new();
        while let Some(ch) = self.ch {
            match ch {
                'a'..='z' | 'A'..='Z' | '_' => {
                    literal.push(ch);
                    self.read_char();
                }
                _ => {
                    break;
                }
            }
        }
        Token {
            t: lookup_ident(literal.as_str()),
            literal,
        }
    }

    fn read_number(&mut self) -> Token {
        let mut literal = String::new();
        while let Some(ch) = self.ch {
            match ch {
                '0'..='9' => {
                    literal.push(ch);
                    self.read_char();
                }
                _ => {
                    break;
                }
            };
        }
        Token {
            t: TokenType::Int,
            literal,
        }
    }
    fn read_string(&mut self) -> String {
        let mut literal = String::new();
        loop {
            self.read_char();
            if self.ch.is_none() || self.ch.unwrap() == '"' {
                break;
            }
            literal.push(self.ch.unwrap());
        }
        literal
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.ch {
            match ch {
                ' ' | '\t' | '\n' | '\r' => self.read_char(),
                _ => {
                    break;
                }
            };
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, TokenType};

    #[test]
    fn test_next_token() {
        let input = r#"let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

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
-a * b
"foobar"
"foo bar"
[1,2];
{"foo": "bar"}
macro(x, y) { x + y; };"#;

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
            (TokenType::LParen, "("),
            (TokenType::Ident, "x"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "y"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Ident, "x"),
            (TokenType::Plus, "+"),
            (TokenType::Ident, "y"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "result"),
            (TokenType::Assign, "="),
            (TokenType::Ident, "add"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "five"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "ten"),
            (TokenType::RParen, ")"),
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
            (TokenType::LParen, "("),
            (TokenType::Int, "5"),
            (TokenType::Lt, "<"),
            (TokenType::Int, "10"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::True, "true"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Else, "else"),
            (TokenType::LBrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::False, "false"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Int, "10"),
            (TokenType::Eq, "=="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "10"),
            (TokenType::NotEq, "!="),
            (TokenType::Int, "9"),
            (TokenType::Semicolon, ";"),
            (TokenType::Minus, "-"),
            (TokenType::Ident, "a"),
            (TokenType::Asterisk, "*"),
            (TokenType::Ident, "b"),
            (TokenType::String, "foobar"),
            (TokenType::String, "foo bar"),
            (TokenType::LBracket, "["),
            (TokenType::Int, "1"),
            (TokenType::Comma, ","),
            (TokenType::Int, "2"),
            (TokenType::RBracket, "]"),
            (TokenType::Semicolon, ";"),
            (TokenType::LBrace, "{"),
            (TokenType::String, "foo"),
            (TokenType::Colon, ":"),
            (TokenType::String, "bar"),
            (TokenType::RBrace, "}"),
            (TokenType::Macro, "macro"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "x"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "y"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Ident, "x"),
            (TokenType::Plus, "+"),
            (TokenType::Ident, "y"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Semicolon, ";"),
        ];

        let mut l = Lexer::new(input);

        for (expected_type, expected_literal) in tests.iter() {
            let tok = l.next().unwrap();
            assert_eq!(tok.t, *expected_type);
            assert_eq!(tok.literal, *expected_literal);
        }
        assert_eq!(l.next(), None);
    }
}
