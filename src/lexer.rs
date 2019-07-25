use super::token::{lookup_ident, Token, TokenType};

pub struct Lexer {
    chars: std::iter::Peekable<std::vec::IntoIter<char>>,
    ch: char,
    eof: bool,
}

impl Lexer {
    pub fn new(input: &String) -> Lexer {
        let mut l = Lexer {
            chars: input
                .chars()
                .collect::<Vec<_>>()
                .into_iter()
                .peekable(),
            ch: '_',
            eof: false,
        };
        l.read_char();
        l
    }

    fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        let tok = if self.eof {
            None
        } else {
            Some(match self.ch {
                '=' => {
                    if self.peek_char() == '=' {
                        let ch = self.ch;
                        self.read_char();
                        let literal: String = vec![ch, self.ch].into_iter().collect();
                        Token {
                            t: TokenType::Eq,
                            literal,
                        }
                    } else {
                        Token {
                            t: TokenType::Assign,
                            literal: self.ch.to_string(),
                        }
                    }
                }
                '+' => Token {
                    t: TokenType::Plus,
                    literal: self.ch.to_string(),
                },
                '-' => Token {
                    t: TokenType::Minus,
                    literal: self.ch.to_string(),
                },
                '!' => {
                    if self.peek_char() == '=' {
                        let ch = self.ch;
                        self.read_char();
                        let literal: String = vec![ch, self.ch].into_iter().collect();
                        Token {
                            t: TokenType::NotEq,
                            literal,
                        }
                    } else {
                        Token {
                            t: TokenType::Bang,
                            literal: self.ch.to_string(),
                        }
                    }
                }
                '*' => Token {
                    t: TokenType::Asterisk,
                    literal: self.ch.to_string(),
                },
                '/' => Token {
                    t: TokenType::Slash,
                    literal: self.ch.to_string(),
                },
                '<' => Token {
                    t: TokenType::Lt,
                    literal: self.ch.to_string(),
                },
                '>' => Token {
                    t: TokenType::Gt,
                    literal: self.ch.to_string(),
                },
                ',' => Token {
                    t: TokenType::Comma,
                    literal: self.ch.to_string(),
                },
                ';' => Token {
                    t: TokenType::Semicolon,
                    literal: self.ch.to_string(),
                },
                '(' => Token {
                    t: TokenType::LParen,
                    literal: self.ch.to_string(),
                },
                ')' => Token {
                    t: TokenType::RParen,
                    literal: self.ch.to_string(),
                },
                '{' => Token {
                    t: TokenType::LBrace,
                    literal: self.ch.to_string(),
                },
                '}' => Token {
                    t: TokenType::RBrace,
                    literal: self.ch.to_string(),
                },
                'a'..='z' | 'A'..='Z' | '_' => return Some(self.read_identifier()),
                '0'..='9' => return Some(self.read_number()),
                _ => Token {
                    t: TokenType::Illegal,
                    literal: self.ch.to_string(),
                },
            })
        };

        self.read_char();
        tok
    }

    fn read_char(&mut self) {
        match self.chars.next() {
            Some(ch) => {
                self.ch = ch;
            }
            None => {
                self.eof = true;
                self.ch = '_'
            }
        }
    }

    fn peek_char(&mut self) -> char {
        match self.chars.peek() {
            Some(ch) => *ch,
            None => {
                self.eof = true;
                '_'
            }
        }
    }

    fn read_identifier(&mut self) -> Token {
        let mut literal = String::new();
        loop {
            match self.ch {
                'a'...'z' | 'A'...'Z' | '_' => {
                    literal.push(self.ch);
                    self.read_char();
                }
                _ => {
                    break;
                }
            };
        }
        Token {
            t: lookup_ident(literal.as_str()),
            literal: literal,
        }
    }

    fn read_number(&mut self) -> Token {
        let mut literal = String::new();
        loop {
            match self.ch {
                '0'..='9' => {
                    literal.push(self.ch);
                    self.read_char();
                }
                _ => {
                    break;
                }
            };
        }
        Token {
            t: TokenType::Int,
            literal: literal,
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.ch {
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
"#
        .to_string();

        let tests = vec![
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
        ];

        let mut l = Lexer::new(&input);

        for tt in &tests {
            let tok = l.next().unwrap();
            assert_eq!(tok.t, tt.0);
            assert_eq!(tok.literal, tt.1);
        }
        assert_eq!(l.next(), None);
    }
}
