use std::mem;

use crate::ast::{BlockStatement, Expression, Program, Statement};
use crate::object::Object;

const NULL: Object = Object::Null;
const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);

pub fn eval_program(program: &Program) -> Object {
    let mut result = NULL;

    for statement in &program.statements {
        result = eval(statement);

        match result {
            Object::ReturnValue(value) => return *value,
            Object::Error(_) => return result,
            _ => continue,
        }
    }

    result
}

fn eval(statement: &Statement) -> Object {
    match statement {
        Statement::ExpressionStatement { expression, .. } => eval_expression(expression),
        Statement::Block(block) => eval_block_statement(block),
        Statement::Return { value, .. } => {
            let value = eval_expression(value);
            if value.is_error() {
                value
            } else {
                Object::ReturnValue(Box::new(value))
            }
        }
        _ => todo!(),
    }
}

fn eval_block_statement(block: &BlockStatement) -> Object {
    let mut result = NULL;

    for statement in &block.statements {
        result = eval(statement);

        if matches!(result, Object::ReturnValue(_)) || matches!(result, Object::Error(_)) {
            return result;
        }
    }

    result
}

fn eval_expression(expression: &Expression) -> Object {
    match expression {
        Expression::IntegerLiteral { value, .. } => Object::from(*value),
        Expression::Boolean { value, .. } => native_bool_to_boolean_object(*value),
        Expression::Prefix {
            operator, right, ..
        } => {
            let right = eval_expression(right);
            if right.is_error() {
                right
            } else {
                eval_prefix_expression(operator, &right)
            }
        }
        Expression::Infix {
            left,
            operator,
            right,
            ..
        } => {
            let left = eval_expression(left);
            if left.is_error() {
                return left;
            }

            let right = eval_expression(right);
            if right.is_error() {
                return right;
            }

            eval_infix_expression(operator, &left, &right)
        }
        Expression::If {
            condition,
            consequence,
            alternative,
            ..
        } => {
            let condition = eval_expression(condition);

            if condition.is_error() {
                return condition;
            }

            if is_truthy(&condition) {
                eval_block_statement(consequence)
            } else if let Some(alternative) = alternative {
                eval_block_statement(alternative)
            } else {
                NULL
            }
        }
        _ => todo!(),
    }
}

fn is_truthy(object: &Object) -> bool {
    match object {
        Object::Null => false,
        Object::Boolean(value) => *value,
        _ => true,
    }
}

fn native_bool_to_boolean_object(input: bool) -> Object {
    input.into()
}

fn eval_prefix_expression(operator: &str, right: &Object) -> Object {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Object::Error(format!(
            "unknown operator {operator}{}",
            right.type_string()
        )),
    }
}

fn eval_bang_operator_expression(right: &Object) -> Object {
    match right {
        Object::Boolean(value) => {
            if *value {
                FALSE
            } else {
                TRUE
            }
        }
        Object::Null => TRUE,
        _ => FALSE,
    }
}

fn eval_minus_prefix_operator_expression(right: &Object) -> Object {
    match right {
        Object::Integer(value) => Object::from(-value),
        _ => Object::Error(format!("unknown operator: -{}", right.type_string())),
    }
}

fn eval_infix_expression(operator: &str, left: &Object, right: &Object) -> Object {
    if mem::discriminant(left) != mem::discriminant(right) {
        return Object::Error(format!(
            "type mismatch: {} {operator} {}",
            left.type_string(),
            right.type_string()
        ));
    }

    match (operator, left, right) {
        (_, Object::Integer(left_val), Object::Integer(right_val)) => {
            eval_integer_infix_expression(operator, *left_val, *right_val)
        }
        ("==", _, _) => native_bool_to_boolean_object(left == right),
        ("!=", _, _) => native_bool_to_boolean_object(left != right),
        _ => Object::Error(format!(
            "unknown operator: {} {operator} {}",
            left.type_string(),
            right.type_string()
        )),
    }
}

fn eval_integer_infix_expression(operator: &str, left: i64, right: i64) -> Object {
    match operator {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => native_bool_to_boolean_object(left < right),
        ">" => native_bool_to_boolean_object(left > right),
        "==" => native_bool_to_boolean_object(left == right),
        "!=" => native_bool_to_boolean_object(left != right),
        _ => Object::Error(format!("unknown operator: INTEGER {operator} INTEGER")),
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

    fn test_boolean_object(object: &Object, expected: bool) {
        match object {
            Object::Boolean(value) => assert_eq!(*value, expected),
            _ => panic!("Object is not Boolean"),
        }
    }

    fn test_null_object(object: &Object) {
        assert!(matches!(object, Object::Null));
    }

    #[test]
    fn integer_expression() {
        let tests = [
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for (input, expected) in tests {
            let evaluated = run_eval(input);
            test_integer_object(&evaluated, expected);
        }
    }

    #[test]
    fn boolean_expression() {
        let tests = [
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];
        for (input, expected) in tests {
            let evaluated = run_eval(input);
            test_boolean_object(&evaluated, expected);
        }
    }

    #[test]
    fn bang_operator() {
        let tests = [
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for (input, expected) in tests {
            let evaluated = run_eval(input);
            test_boolean_object(&evaluated, expected);
        }
    }

    #[test]
    fn if_else_expressions() {
        let tests = [
            ("if (true) { 10 }", Some(10)),
            ("if (false) { 10 }", None),
            ("if (1) { 10 }", Some(10)),
            ("if (1 < 2) { 10 }", Some(10)),
            ("if (1 > 2) { 10 }", None),
            ("if (1 > 2) { 10 } else { 20 }", Some(20)),
            ("if (1 < 2) { 10 } else { 20 }", Some(10)),
        ];

        for (input, expected) in tests {
            let evaluated = run_eval(input);
            if let Some(value) = expected {
                test_integer_object(&evaluated, value);
            } else {
                test_null_object(&evaluated);
            }
        }
    }

    #[test]
    fn return_statements() {
        let tests = [
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                r#"
if (10 > 1) { 
    if (10 > 1) {
        return 10; 
    }
    return 1; 
}
            "#,
                10,
            ),
        ];

        for (input, expected) in tests {
            let evaluated = run_eval(input);
            test_integer_object(&evaluated, expected);
        }
    }

    #[test]
    fn error_handling() {
        let tests = [
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                r#"
if (10 > 1) {
  if (10 > 1) {
    return true + false;
  }
  return 1; 
}"#,
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
        ];

        for (input, expected) in tests {
            let evaluated = run_eval(input);

            match evaluated {
                Object::Error(message) => assert_eq!(message, expected),
                _ => panic!("No error object returned! Got {evaluated:?}"),
            }
        }
    }
}
