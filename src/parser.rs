use std::fmt;
use super::token::{Token, TokenType};
use super::lexer::Lexer;
use super::ast::*;

#[derive(Debug)]
struct ParseError {
    message: String
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for ParseError {}

struct Parser<'a> {
    l: std::iter::Peekable<Lexer<'a>>,
    errors: Vec<ParseError>
}

impl Parser<'_> {
    fn new(l: Lexer) -> Parser {
        Parser {
            l: l.peekable(),
            errors: vec![]
        }
    }

    fn parse_program(&mut self) -> Program {
        let mut statements: Vec<Statement> = vec![];

        while let Some(cur_token) = self.l.next() {
            match self.parse_statement(cur_token) {
                Ok(stmt) => { statements.push(stmt) }
                Err(error) => { self.errors.push(error) }
            }
        }
        Program { statements }
    }

    fn parse_statement(&mut self, cur_token: Token) -> Result<Statement, ParseError> {
        Ok(
            match cur_token.t {
                TokenType::Let => Statement::LetStatement(self.parse_let_statement()?),
                _ => return Err(ParseError{message: String::from("not implemented")})
            }
        )
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement, ParseError> {
        let ident_token = self.expect_peek(TokenType::Ident)?;

        self.expect_peek(TokenType::Assign)?;
        // TODO: セミコロンまで読み飛ばしている
        while let Some(next) = self.l.next() {
            if next.t == TokenType::Semicolon { break; }
        }
        let identifier = Identifier {value: ident_token.literal};

        Ok(LetStatement{identifier})
    }

    fn expect_peek(&mut self, t: TokenType) -> Result<Token, ParseError> {
        match self.l.peek() {
            Some(peek) => {
                if peek.t == t {
                     self.l.next().ok_or(ParseError {message: String::from("Unexpected EOF") })
                } else {
                    Err(ParseError {
                        message: format!("expected next token to be {:?}, got {:?} instead", t, peek.t)
                    })
                }
            },
            None => Err(ParseError {message: String::from("Unexpected EOF") })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, Parser};
    use super::super::ast::*;

    #[test]
    fn test_let_statements() {
        let input = r#"
let x 5;
let y = 10;
let foobar = 838383;
"#.to_string();

        let l = Lexer::new(&input);
        let mut p = Parser::new(l);

        let program = p. parse_program();
        check_parse_errors(p);
        assert_eq!(program.statements.len(), 3);

        let tests = [
            "x",
            "y",
            "foobar"
        ];

        for (i, expected) in tests.iter().enumerate() {
            let stmt = &program.statements[i];
            assert_let_statement(stmt, expected);
        }
    }

    fn check_parse_errors(p: Parser) {
        let errors = p.errors;
        let len = errors.len();

        if len == 0 { return }
        for e in errors { eprintln!("parser error: {}", e); }
        assert!(false, "parser has {} errors.", len);
    }

    fn assert_let_statement(statement: &Statement, expected: &str) {
        assert_eq!(statement, &Statement::LetStatement(LetStatement{identifier: Identifier{value: expected.to_string()}}));
    }
}
