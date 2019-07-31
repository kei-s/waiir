use super::ast::*;
use super::object::*;

trait Eval {
    fn eval(&self) -> Object;
}

impl Eval for Program {
    fn eval(&self) -> Object {
        self.statements.eval()
    }
}

impl Eval for Vec<Statement> {
    fn eval(&self) -> Object {
        let mut result = Object::Null;
        for stmt in self {
            result = stmt.eval();
        }
        result
    }
}

impl Eval for Statement {
    fn eval(&self) -> Object {
        match self {
            Statement::ExpressionStatement(stmt) => stmt.expression.eval(),
            _ => panic!(),
        }
    }
}

impl Eval for Expression {
    fn eval(&self) -> Object {
        match self {
            Expression::IntegerLiteral(exp) => exp.eval(),
            _ => panic!(),
        }
    }
}

impl Eval for IntegerLiteral {
    fn eval(&self) -> Object {
        Object::Integer(self.value)
    }
}

#[cfg(test)]
mod tests {
    use super::super::lexer::Lexer;
    use super::super::object::*;
    use super::super::parser::Parser;
    use super::Eval;

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![("5", 5), ("10", 10)];

        for tt in tests {
            let input = tt.0.to_string();
            let expected = tt.1;
            let evaluated = test_eval(&input);
            assert_integer_object(evaluated, expected);
        }
    }

    fn test_eval(input: &String) -> Object {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        program.eval()
    }

    fn assert_integer_object(obj: Object, expected: i64) {
        if let Object::Integer(result) = obj {
            assert_eq!(result, expected)
        } else {
            assert!(false, "object is not Integer")
        }
    }
}
