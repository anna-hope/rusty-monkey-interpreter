use crate::ast::{Expression, Program, Statement};
use crate::object::Object;

pub fn eval_program(program: &Program) -> Object {
    let mut result = Object::default();
    for statement in program.statements.iter() {
        result = eval(statement);
    }
    result
}

fn eval(statement: &Statement) -> Object {
    match statement {
        Statement::ExpressionStatement { expression, .. } => eval_expression(expression),
        _ => todo!(),
    }
}

fn eval_expression(expression: &Expression) -> Object {
    match expression {
        Expression::IntegerLiteral { value, .. } => Object::Integer(*value),
        _ => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn run_eval(input: &str) -> Object {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        eval_program(&program)
    }

    fn test_integer_object(object: &Object, expected: i64) {
        match object {
            Object::Integer(value) => assert_eq!(*value, expected),
            _ => panic!("Object is not Integer"),
        }
    }

    #[test]
    fn eval_integer_expression() {
        let tests = [("5", 5), ("10", 10)];

        for (input, expected) in tests {
            let evaluated = run_eval(input);
            test_integer_object(&evaluated, expected);
        }
    }
}
