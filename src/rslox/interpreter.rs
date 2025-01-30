use super::InterpreterError;
use crate::rslox::parser;
use crate::rslox::parser::{Expr, Stmt};

pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter
    }

    pub fn run(&self, statements: &Vec<parser::Stmt>) -> Result<(), InterpreterError> {
        for statement in statements {
            self.interpret_statement(statement)?
        }
        Ok(())
    }

    fn interpret_statement(&self, statement: &parser::Stmt) -> Result<(), InterpreterError> {
        match statement {
            Stmt::Expression { .. } => return Ok(()),
            Stmt::Print { expression } => {
                let value = self.interpret_expression(expression);
                println!("{}", value.unwrap());
                return Ok(());
            }
        }
    }

    fn interpret_expression(
        &self,
        expression: &parser::Expr,
    ) -> Result<LoxValue, InterpreterError> {
        match expression {
            Expr::Binary {
                left,
                operator,
                right,
            } => self.interpret_binary(left, operator, right),
            Expr::Grouping { expression } => self.interpret_expression(expression),
            Expr::Literal { value } => self.interpret_literal(value),
            Expr::Unary { operator, right } => self.interpret_unary(operator, right),
        }
    }

    fn interpret_literal(
        &self,
        literal: &parser::LoxParserValue,
    ) -> Result<LoxValue, InterpreterError> {
        match literal {
            parser::LoxParserValue::Number(value) => Ok(LoxValue::Number(value.clone())),
            parser::LoxParserValue::String(value) => Ok(LoxValue::String(value.clone())),
            parser::LoxParserValue::Boolean(value) => Ok(LoxValue::Boolean(value.clone())),
            parser::LoxParserValue::Nil => Ok(LoxValue::Nil),
        }
    }

    fn interpret_binary(
        &self,
        left: &parser::Expr,
        operator: &parser::LoxBinaryOperator,
        right: &parser::Expr,
    ) -> Result<LoxValue, InterpreterError> {
        let left = self.interpret_expression(left)?;
        let right = self.interpret_expression(right)?;

        match operator {
            parser::LoxBinaryOperator::Plus => {
                if let (LoxValue::Number(left), LoxValue::Number(right)) = (left, right) {
                    Ok(LoxValue::Number(left + right))
                } else {
                    Err(InterpreterError::RuntimeError(
                        "Operands must be numbers".to_string(),
                    ))
                }
            }
            parser::LoxBinaryOperator::Minus => {
                if let (LoxValue::Number(left), LoxValue::Number(right)) = (left, right) {
                    Ok(LoxValue::Number(left - right))
                } else {
                    Err(InterpreterError::RuntimeError(
                        "Operands must be numbers".to_string(),
                    ))
                }
            }
            parser::LoxBinaryOperator::Star => {
                if let (LoxValue::Number(left), LoxValue::Number(right)) = (left, right) {
                    Ok(LoxValue::Number(left * right))
                } else {
                    Err(InterpreterError::RuntimeError(
                        "Operands must be numbers".to_string(),
                    ))
                }
            }
            parser::LoxBinaryOperator::Slash => {
                if let (LoxValue::Number(left), LoxValue::Number(right)) = (left, right) {
                    Ok(LoxValue::Number(left / right))
                } else {
                    Err(InterpreterError::RuntimeError(
                        "Operands must be numbers".to_string(),
                    ))
                }
            }
            _ => todo!(),
        }
    }

    fn interpret_unary(
        &self,
        operator: &parser::LoxUnaryOperator,
        right: &parser::Expr,
    ) -> Result<LoxValue, InterpreterError> {
        todo!();
    }
}

enum LoxValue {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl std::fmt::Display for LoxValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxValue::Number(value) => write!(f, "{}", value),
            LoxValue::String(value) => write!(f, "{}", value),
            LoxValue::Boolean(value) => write!(f, "{}", value),
            LoxValue::Nil => write!(f, "nil"),
        }
    }
}
