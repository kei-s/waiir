use std::fmt;
use super::token::{Token, TokenType};
use super::lexer::Lexer;
use super::ast::*;

enum Precedence {
    Lowest,
    Equals, // ==
    LessGreater, // > OR <
    Sum, // +
    Product, // *
    Prefix, // -X OR !X
    Call // myFunction(x)
}

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

        while let Ok(cur_token) = self.next_token() {
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
                TokenType::Return => Statement::ReturnStatement(self.parse_return_statement()?),
                _ => Statement::ExpressionStatement(self.parse_expression_statement(cur_token)?)
            }
        )
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement, ParseError> {
        let ident_token = self.expect_peek(TokenType::Ident)?;

        self.expect_peek(TokenType::Assign)?;
        // TODO: セミコロンまで読み飛ばしている
        while let Ok(next) = self.next_token() {
            if next.t == TokenType::Semicolon { break; }
        }
        let name = Identifier {value: ident_token.literal};

        Ok(LetStatement{name})
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement, ParseError> {
        self.next_token()?;

        // TODO: セミコロンまで読み飛ばしている
        while let Ok(next) = self.next_token() {
            if next.t == TokenType::Semicolon { break; }
        }

        Ok(ReturnStatement{})
    }

    fn parse_expression_statement(&mut self, cur_token: Token) -> Result<ExpressionStatement, ParseError> {
        let expression = self.parse_expression(cur_token, Precedence::Lowest)?;

        if let Some(token) = self.peek_token() {
            if token.t == TokenType::Semicolon { self.next_token()?; }
        }

        Ok(ExpressionStatement{expression})
    }

    fn parse_expression(&mut self, cur_token: Token, precedence: Precedence) -> Result<Expression, ParseError> {
        Ok(
            match cur_token.t {
                TokenType::Ident => Expression::Identifier(self.parse_identifier(cur_token)),
                TokenType::Int => Expression::IntegerLiteral(self.parse_integer_literal(cur_token)?),
                TokenType::Bang => Expression::PrefixExpression(self.parse_prefix_expression(cur_token)?),
                TokenType::Minus => Expression::PrefixExpression(self.parse_prefix_expression(cur_token)?),
                _ => return Err(ParseError {message: String::from("not implemented") })
            }
        )
    }

    fn parse_identifier(&self, cur_token: Token) -> Identifier {
        Identifier { value: cur_token.literal }
    }

    fn parse_integer_literal(&self, cur_token: Token) -> Result<IntegerLiteral, ParseError> {
        match cur_token.literal.parse() {
            Ok(value) => Ok(IntegerLiteral { value }),
            Err(_) =>
                Err(ParseError {
                    message: format!("could not parse {} as integer", cur_token.literal)
                })
        }
    }

    fn parse_prefix_expression(&mut self, cur_token: Token) -> Result<PrefixExpression, ParseError> {
        let next_token = self.next_token()?;
        let right = self.parse_expression(next_token, Precedence::Prefix)?;
        Ok(PrefixExpression { operator: cur_token.literal, right: Box::new(right) })
    }

    fn next_token(&mut self) -> Result<Token, ParseError> {
        self.l.next().ok_or(ParseError {message: String::from("Unexpected EOF") })
    }
    fn peek_token(&mut self) -> Option<&Token> { self.l.peek() }

    fn expect_peek(&mut self, t: TokenType) -> Result<Token, ParseError> {
        match self.peek_token() {
            Some(peek) => {
                if peek.t == t {
                     self.next_token()
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
let x = 5;
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

    #[test]
    fn test_return_statements() {
        let input = r#"
return 5;
return 10;
return 993322;
"#.to_string();

        let l = Lexer::new(&input);
        let mut p = Parser::new(l);

        let program = p. parse_program();
        check_parse_errors(p);
        assert_eq!(program.statements.len(), 3);
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;".to_string();

        let l = Lexer::new(&input);
        let mut p = Parser::new(l);

        let program = p. parse_program();
        check_parse_errors(p);
        assert_eq!(program.statements.len(), 1);

        if let Statement::ExpressionStatement(exp_stmt) = &program.statements[0] {
            if let Expression::Identifier(ident) = &exp_stmt.expression {
                assert_eq!(ident.value, "foobar");
            } else {
                assert!(false, "program.statements[0] is not ast::Identifier")
            }
        } else {
            assert!(false, "program.statements[0] is not ast::ExpressionStatement")
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;".to_string();

        let l = Lexer::new(&input);
        let mut p = Parser::new(l);

        let program = p. parse_program();
        check_parse_errors(p);
        assert_eq!(program.statements.len(), 1);

        if let Statement::ExpressionStatement(exp_stmt) = &program.statements[0] {
            if let Expression::IntegerLiteral(literal) = &exp_stmt.expression {
                assert_eq!(literal.value, 5);
            } else {
                assert!(false, "program.statements[0] is not ast::IntegerLiteral")
            }
        } else {
            assert!(false, "program.statements[0] is not ast::ExpressionStatement")
        }
    }

    #[test]
    fn test_parsing_prefix_expression() {
        struct Test<'a> {
            input: &'a str,
            operator: &'a str,
            integer_value: isize,
        }

        let prefix_tests = vec![
            Test{input: "!5;", operator: "!", integer_value: 5},
            Test{input: "-15;", operator: "-", integer_value: 15},
        ];

        for tt in prefix_tests {
            let input = tt.input.to_string();
            let l = Lexer::new(&input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parse_errors(p);

            assert_eq!(program.statements.len(), 1);
            if let Statement::ExpressionStatement(stmt) = &program.statements[0] {
                if let Expression::PrefixExpression(exp) = &stmt.expression {
                    assert_eq!(exp.operator, tt.operator);
                    assert_integer_literal(&*exp.right, tt.integer_value);
                } else {
                    assert!(false, "stmt is not ast::PrefixExpression");
                }
            } else {
                assert!(false, "program.statements[0] is not ast::ExpressionStatement");
            }
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
        assert_eq!(statement, &Statement::LetStatement(LetStatement{name: Identifier{value: expected.to_string()}}));
    }

    fn assert_integer_literal(il: &Expression, value: isize) {
        if let Expression::IntegerLiteral(integ) = il {
            assert_eq!(integ.value, value);
            assert_eq!(format!("{}", integ.value), format!("{}", value));
        } else {
            assert!(false, "il not ast::IntegerLiteral");
        }
    }
}
