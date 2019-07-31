use super::ast::*;
use super::object::*;

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

pub trait Eval {
    fn eval(&self) -> Object;
}

macro_rules! impl_eval {
    ($ty:ty, $self:ident, $block:block) => {
        impl Eval for $ty {
            fn eval(&$self) -> Object {
                $block
            }
        }
    };
}

impl_eval!(Program, self, { self.statements.eval() });

impl_eval!(Vec<Statement>, self, {
    let mut result = Object::Null;
    for stmt in self {
        result = stmt.eval();
    }
    result
});

impl_eval!(Statement, self, {
    match self {
        Statement::ExpressionStatement(stmt) => stmt.expression.eval(),
        _ => panic!(),
    }
});

impl_eval!(Expression, self, {
    match self {
        Expression::IntegerLiteral(exp) => exp.eval(),
        Expression::Boolean(exp) => exp.eval(),
        Expression::PrefixExpression(exp) => exp.eval(),
        _ => panic!(),
    }
});

impl_eval!(IntegerLiteral, self, { Object::Integer(self.value) });

impl_eval!(Boolean, self, { native_bool_to_boolean_object(self.value) });

fn native_bool_to_boolean_object(input: bool) -> Object {
    if input {
        TRUE
    } else {
        FALSE
    }
}

impl_eval!(PrefixExpression, self, {
    let right = self.right.eval();
    eval_prefix_expression(&self.operator, right)
});

fn eval_prefix_expression(operator: &String, right: Object) -> Object {
    match operator.as_str() {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Object::Null,
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        TRUE => FALSE,
        FALSE => TRUE,
        NULL => TRUE,
        _ => FALSE,
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    if let Object::Integer(value) = right {
        Object::Integer(-value)
    } else {
        NULL
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
        let tests = vec![("5", 5), ("10", 10), ("-5", -5), ("-10", -10)];

        for tt in tests {
            let input = tt.0.to_string();
            let expected = tt.1;
            let evaluated = test_eval(&input);
            assert_integer_object(evaluated, expected);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![("true", true), ("false", false)];

        for tt in tests {
            let input = tt.0.to_string();
            let expected = tt.1;
            let evaluated = test_eval(&input);
            assert_boolean_object(evaluated, expected);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for tt in tests {
            let input = tt.0.to_string();
            let expected = tt.1;
            let evaluated = test_eval(&input);
            assert_boolean_object(evaluated, expected);
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

    fn assert_boolean_object(obj: Object, expected: bool) {
        if let Object::Boolean(result) = obj {
            assert_eq!(result, expected)
        } else {
            assert!(false, "object is not Boolean")
        }
    }
}
