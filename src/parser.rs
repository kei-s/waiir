use super::token::{Token, TokenType};
use super::lexer::Lexer;
use super::ast::*;

struct Parser<'a> {
    l: std::iter::Peekable<Lexer<'a>>,
}

impl Parser<'_> {
    fn new(l: Lexer) -> Parser {
        Parser { l: l.peekable() }
    }

    fn parse_program(&mut self) -> Program {
        let mut statements: Vec<Statement> = vec![];

        while let Some(cur_token) = self.l.next() {
            let stmt = self.parse_statement(cur_token);
            statements.push(stmt);
        }
        Program { statements }
    }

    fn parse_statement(&mut self, cur_token: Token) -> Statement {
        match cur_token.t {
            TokenType::Let => Statement::LetStatement(self.parse_let_statement(cur_token).unwrap()),
            _ => panic!()
        }
    }

    fn parse_let_statement(&mut self, cur_token: Token) -> Option<LetStatement> {
        let ident_token = match self.expect_peek(TokenType::Ident) {
            Some(token) => token,
            None => return None
        };

        return match self.expect_peek(TokenType::Assign) {
            Some(_) => {
                // TODO: セミコロンまで読み飛ばしている
                while let Some(next) = self.l.next() {
                    if next.t == TokenType::Semicolon { break; }
                }
                Some(LetStatement{identifier: Identifier {value: ident_token.literal}})
            },
            None => None
        }
    }

    fn expect_peek(&mut self, t: TokenType) -> Option<Token> {
        match self.l.peek() {
            Some(peek) => {
                if peek.t == t { self.l.next() }
                else { None }
            },
            None => None
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
let x = 5;
let y = 10;
let foobar = 838383;
"#.to_string();

        let l = Lexer::new(&input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
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

    fn assert_let_statement(statement: &Statement, expected: &str) {
        assert_eq!(statement, &Statement::LetStatement(LetStatement{identifier: Identifier{value: expected.to_string()}}));
    }
}
