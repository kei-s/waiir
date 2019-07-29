use super::ast::*;
use super::lexer::Lexer;
use super::token::{Token, TokenType};
use std::fmt;

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > OR <
    Sum,         // +
    Product,     // *
    Prefix,      // -X OR !X
    Call,        // myFunction(x)
}

#[derive(Debug)]
struct ParseError {
    message: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for ParseError {}

struct Parser {
    l: Lexer,
    _cur_token: Option<Token>,
    _peek_token: Option<Token>,
    errors: Vec<ParseError>,
}

impl Parser {
    fn new(mut l: Lexer) -> Parser {
        let _cur_token = l.next();
        let _peek_token = l.next();

        Parser {
            l,
            _cur_token,
            _peek_token,
            errors: vec![],
        }
    }

    fn parse_program(&mut self) -> Program {
        let mut statements: Vec<Statement> = vec![];

        while self._cur_token.is_some() {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(error) => self.errors.push(error),
            }
            self.next_token();
        }
        Program { statements }
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        Ok(match self.cur_token().t {
            TokenType::Let => Statement::LetStatement(self.parse_let_statement()?),
            TokenType::Return => Statement::ReturnStatement(self.parse_return_statement()?),
            _ => Statement::ExpressionStatement(self.parse_expression_statement()?),
        })
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement, ParseError> {
        self.expect_peek(&TokenType::Ident);

        let name = Identifier {
            value: self.cur_token().literal.clone(),
        };

        self.expect_peek(&TokenType::Assign);

        // TODO: セミコロンまで読み飛ばしている
        while !self.cur_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Ok(LetStatement { name })
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement, ParseError> {
        self.next_token();

        // TODO: セミコロンまで読み飛ばしている
        while !self.cur_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Ok(ReturnStatement {})
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStatement, ParseError> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token()
        }

        Ok(ExpressionStatement { expression })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
        let mut left_exp = match self.cur_token().t {
            TokenType::Ident => Expression::Identifier(self.parse_identifier()),
            TokenType::Int => Expression::IntegerLiteral(self.parse_integer_literal()?),
            TokenType::Bang | TokenType::Minus => {
                Expression::PrefixExpression(self.parse_prefix_expression()?)
            }
            _ => {
                return Err(ParseError {
                    message: String::from("not implemented"),
                })
            }
        };

        loop {
            if !self.peek_token_is(&TokenType::Semicolon) && precedence < self.peek_precedence() {
                left_exp = match self.peek_token().unwrap().t {
                    TokenType::Plus
                    | TokenType::Minus
                    | TokenType::Slash
                    | TokenType::Asterisk
                    | TokenType::Eq
                    | TokenType::NotEq
                    | TokenType::Lt
                    | TokenType::Gt => {
                        self.next_token();
                        Expression::InfixExpression(self.parse_infix_expression(left_exp)?)
                    }
                    _ => return Ok(left_exp),
                };
            } else {
                break;
            }
        }

        Ok(left_exp)
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<InfixExpression, ParseError> {
        let operator = self.cur_token().literal.clone();
        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;
        Ok(InfixExpression {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })
    }

    fn parse_identifier(&self) -> Identifier {
        Identifier {
            value: self.cur_token().literal.clone(),
        }
    }

    fn parse_integer_literal(&self) -> Result<IntegerLiteral, ParseError> {
        match self.cur_token().literal.parse() {
            Ok(value) => Ok(IntegerLiteral { value }),
            Err(_) => Err(ParseError {
                message: format!("could not parse {} as integer", self.cur_token().literal),
            }),
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<PrefixExpression, ParseError> {
        let operator = self.cur_token().literal.clone();

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix)?;

        Ok(PrefixExpression {
            operator,
            right: Box::new(right),
        })
    }

    fn next_token(&mut self) {
        self._cur_token = self._peek_token.take();
        self._peek_token = self.l.next();
    }

    fn cur_token(&self) -> &Token {
        self._cur_token.as_ref().unwrap()
    }

    fn peek_token(&self) -> Option<&Token> {
        self._peek_token.as_ref()
    }

    fn cur_token_is(&self, t: &TokenType) -> bool {
        &self.cur_token().t == t
    }

    fn peek_token_is(&self, t: &TokenType) -> bool {
        self.peek_token().is_some() && &self.peek_token().unwrap().t == t
    }

    fn expect_peek(&mut self, t: &TokenType) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            self.errors.push(ParseError {
                message: format!(
                    "expected next token to be {:?}, got {:?} instead",
                    t,
                    self.peek_token().unwrap().t
                ),
            });
            false
        }
    }

    fn cur_precedence(&self) -> Precedence {
        Parser::precedence(&self.cur_token().t)
    }

    fn peek_precedence(&self) -> Precedence {
        if let Some(peek) = self.peek_token() {
            Parser::precedence(&peek.t)
        } else {
            Precedence::Lowest
        }
    }

    fn precedence(t: &TokenType) -> Precedence {
        match t {
            TokenType::Eq => Precedence::Equals,
            TokenType::NotEq => Precedence::Equals,
            TokenType::Lt => Precedence::LessGreater,
            TokenType::Gt => Precedence::LessGreater,
            TokenType::Plus => Precedence::Sum,
            TokenType::Minus => Precedence::Sum,
            TokenType::Slash => Precedence::Product,
            TokenType::Asterisk => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::ast::*;
    use super::{Lexer, Parser};

    #[test]
    fn test_let_statements() {
        let input = r#"
let x = 5;
let y = 10;
let foobar = 838383;
"#
        .to_string();

        let l = Lexer::new(&input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parse_errors(p);
        assert_eq!(program.statements.len(), 3);

        let tests = ["x", "y", "foobar"];

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
"#
        .to_string();

        let l = Lexer::new(&input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parse_errors(p);
        assert_eq!(program.statements.len(), 3);
    }

    #[test]
    fn test_identifier_expressions() {
        let input = "foobar;".to_string();

        let l = Lexer::new(&input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parse_errors(p);
        assert_eq!(program.statements.len(), 1);

        if let Statement::ExpressionStatement(exp_stmt) = &program.statements[0] {
            if let Expression::Identifier(ident) = &exp_stmt.expression {
                assert_eq!(ident.value, "foobar");
            } else {
                assert!(false, "program.statements[0] is not ast::Identifier")
            }
        } else {
            assert!(
                false,
                "program.statements[0] is not ast::ExpressionStatement"
            )
        }
    }

    #[test]
    fn test_integer_literal_expressions() {
        let input = "5;".to_string();

        let l = Lexer::new(&input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parse_errors(p);
        assert_eq!(program.statements.len(), 1);

        if let Statement::ExpressionStatement(exp_stmt) = &program.statements[0] {
            if let Expression::IntegerLiteral(literal) = &exp_stmt.expression {
                assert_eq!(literal.value, 5);
            } else {
                assert!(false, "program.statements[0] is not ast::IntegerLiteral")
            }
        } else {
            assert!(
                false,
                "program.statements[0] is not ast::ExpressionStatement"
            )
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests = vec![("!5;", "!", 5), ("-15;", "-", 15)];

        for tt in prefix_tests {
            let input = tt.0.to_string();
            let expected_operator = tt.1;
            let expected_integer_value = tt.2;

            let l = Lexer::new(&input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parse_errors(p);

            assert_eq!(program.statements.len(), 1);
            if let Statement::ExpressionStatement(stmt) = &program.statements[0] {
                if let Expression::PrefixExpression(exp) = &stmt.expression {
                    assert_eq!(exp.operator, expected_operator);
                    assert_literal_expression(&*exp.right, expected_integer_value);
                } else {
                    assert!(false, "stmt is not ast::PrefixExpression");
                }
            } else {
                assert!(
                    false,
                    "program.statements[0] is not ast::ExpressionStatement"
                );
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let infix_tests = vec![
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
            // ("a != 5;", "a".to_string(), "!=", 5),
        ];

        for tt in infix_tests {
            let input = tt.0.to_string();
            let expected_left_value = tt.1;
            let expected_operator = tt.2;
            let expected_right_value = tt.3;

            let l = Lexer::new(&input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parse_errors(p);

            assert_eq!(program.statements.len(), 1);
            if let Statement::ExpressionStatement(stmt) = &program.statements[0] {
                assert_infix_expression(
                    &stmt.expression,
                    expected_left_value,
                    expected_operator,
                    expected_right_value,
                );
            } else {
                assert!(
                    false,
                    "program.statements[0] is not ast::ExpressionStatement"
                );
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];

        for tt in tests {
            let input = tt.0.to_string();
            let expected = tt.1;

            let l = Lexer::new(&input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parse_errors(p);

            assert_eq!(format!("{}", program), expected);
        }
    }

    fn check_parse_errors(p: Parser) {
        let errors = p.errors;
        let len = errors.len();

        if len == 0 {
            return;
        }
        for e in errors {
            eprintln!("parser error: {}", e);
        }
        assert!(false, "parser has {} errors.", len);
    }

    fn assert_let_statement(statement: &Statement, expected: &str) {
        assert_eq!(
            statement,
            &Statement::LetStatement(LetStatement {
                name: Identifier {
                    value: expected.to_string()
                }
            })
        );
    }

    fn assert_integer_literal(il: &Expression, value: isize) {
        if let Expression::IntegerLiteral(integ) = il {
            assert_eq!(integ.value, value);
            assert_eq!(format!("{}", integ.value), format!("{}", value));
        } else {
            assert!(false, "il not ast::IntegerLiteral");
        }
    }

    fn assert_identifier(exp: &Expression, value: &String) {
        if let Expression::Identifier(ident) = exp {
            assert_eq!(&ident.value, value);
            assert_eq!(format!("{}", ident.value), format!("{}", value));
        } else {
            assert!(false, "exp not ast::Identifier");
        }
    }

    trait AssertWithExpression {
        fn assert_with(self, exp: &Expression);
    }

    impl AssertWithExpression for isize {
        fn assert_with(self, exp: &Expression) {
            assert_integer_literal(exp, self);
        }
    }

    impl AssertWithExpression for String {
        fn assert_with(self, exp: &Expression) {
            assert_identifier(exp, &self);
        }
    }

    fn assert_literal_expression<T: AssertWithExpression>(exp: &Expression, expected: T) {
        expected.assert_with(exp);
    }

    fn assert_infix_expression<T: AssertWithExpression, U: AssertWithExpression>(
        exp: &Expression,
        left: T,
        operator: &str,
        right: U,
    ) {
        if let Expression::InfixExpression(op_exp) = exp {
            assert_literal_expression(&*op_exp.left, left);
            assert_eq!(&op_exp.operator, operator);
            assert_literal_expression(&*op_exp.right, right);
        } else {
            assert!(false, "exp not ast::InfixExpression");
        }
    }
}
