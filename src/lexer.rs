use super::token;
use super::token::{Token,TokenType};

struct Lexer<'a> {
    input: &'a String,
    char_indices: std::iter::Peekable<std::str::CharIndices<'a>>,
    ch: char,
    pos: usize,
    eof: bool,
}

impl<'a> Lexer<'a> {
    fn new(input: &String) -> Lexer {
        let mut l = Lexer{input, char_indices: input.char_indices().peekable(), ch: '_', pos: 0, eof: false};
        l.read_char();
        l
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = if self.eof {
            Token { t: TokenType::Eof, literal: String::from("") }
        } else {
            match self.ch {
                '=' => Token { t: TokenType::Assign, literal: self.ch.to_string() },
                ';' => Token { t: TokenType::Semicolon, literal: self.ch.to_string() },
                '(' => Token { t: TokenType::LParen, literal: self.ch.to_string() },
                ')' => Token { t: TokenType::RParen, literal: self.ch.to_string() },
                ',' => Token { t: TokenType::Comma, literal: self.ch.to_string() },
                '+' => Token { t: TokenType::Plus, literal: self.ch.to_string() },
                '{' => Token { t: TokenType::LBrace, literal: self.ch.to_string() },
                '}' => Token { t: TokenType::RBrace, literal: self.ch.to_string() },
                'a'..='z' | 'A'..='Z' | '_' => return self.read_identifier(),
                '0'..='9' => return self.read_number(),
                _ => Token { t:  TokenType::Illegal, literal: self.ch.to_string() }
            }
        };

        self.read_char();
        tok
    }

    fn read_char(&mut self) {
        match self.char_indices.next() {
            Some((pos, ch)) => { self.pos = pos; self.ch = ch; },
            None => self.eof = true
        }
    }

    fn read_identifier(&mut self) -> Token {
        let start = self.pos;
        loop {
            match self.ch {
                'a'...'z' | 'A'...'Z' | '_' => { self.read_char(); }
                _ =>  { break; }
            };
        }
        let literal = &self.input[start..self.pos];
        Token { t: token::lookup_ident(literal), literal: literal.to_string() }
    }

    fn read_number(&mut self) -> Token {
        let start = self.pos;
        loop {
            match self.ch {
                '0'..='9' => { self.read_char(); }
                _ =>  { break; }
            };
        }
        let literal = &self.input[start..self.pos];
        Token { t: TokenType::Int, literal: literal.to_string() }
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.ch {
                ' ' | '\t' | '\n' | '\r' => self.read_char(),
                _ =>  { break; }
            };
        }
    }
}

#[cfg(test)]
mod tests {
    use super::TokenType;
    use super::Lexer;

    #[test]
    fn test_next_token() {
        let input = r#"let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
"#.to_string();

        struct Expected<'a> {
            t: TokenType,
            literal: &'a str
        }

        let tests = vec![
            Expected { t: TokenType::Let, literal: "let" },
            Expected { t: TokenType::Ident, literal: "five" },
            Expected { t: TokenType::Assign, literal: "=" },
            Expected { t: TokenType::Int, literal: "5" },
            Expected { t: TokenType::Semicolon, literal: ";" },
            Expected { t: TokenType::Let, literal: "let" },
            Expected { t: TokenType::Ident, literal: "ten" },
            Expected { t: TokenType::Assign, literal: "=" },
            Expected { t: TokenType::Int, literal: "10" },
            Expected { t: TokenType::Semicolon, literal: ";" },
            Expected { t: TokenType::Let, literal: "let" },
            Expected { t: TokenType::Ident, literal: "add" },
            Expected { t: TokenType::Assign, literal: "=" },
            Expected { t: TokenType::Function, literal: "fn" },
            Expected { t: TokenType::LParen, literal: "(" },
            Expected { t: TokenType::Ident, literal: "x" },
            Expected { t: TokenType::Comma, literal: "," },
            Expected { t: TokenType::Ident, literal: "y" },
            Expected { t: TokenType::RParen, literal: ")" },
            Expected { t: TokenType::LBrace, literal: "{" },
            Expected { t: TokenType::Ident, literal: "x" },
            Expected { t: TokenType::Plus, literal: "+" },
            Expected { t: TokenType::Ident, literal: "y" },
            Expected { t: TokenType::Semicolon, literal: ";" },
            Expected { t: TokenType::RBrace, literal: "}" },
            Expected { t: TokenType::Semicolon, literal: ";" },
            Expected { t: TokenType::Let, literal: "let" },
            Expected { t: TokenType::Ident, literal: "result" },
            Expected { t: TokenType::Assign, literal: "=" },
            Expected { t: TokenType::Ident, literal: "add" },
            Expected { t: TokenType::LParen, literal: "(" },
            Expected { t: TokenType::Ident, literal: "five" },
            Expected { t: TokenType::Comma, literal: "," },
            Expected { t: TokenType::Ident, literal: "ten" },
            Expected { t: TokenType::RParen, literal: ")" },
            Expected { t: TokenType::Semicolon, literal: ";" }
        ];

        let mut l = Lexer::new(&input);

        for expected in &tests {
            let tok = l.next_token();
            assert_eq!(tok.t, expected.t);
            assert_eq!(tok.literal, expected.literal);
        }
    }
}

