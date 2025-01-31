use ordered_float::OrderedFloat;

use super::InterpreterError;
use crate::rslox::parser;
use crate::rslox::parser::{Expr, Stmt};

pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter
    }

    pub fn run(&self, statements: &[parser::Stmt]) -> Result<(), InterpreterError> {
        for statement in statements {
            self.interpret_statement(statement)?
        }
        Ok(())
    }

    fn interpret_statement(&self, statement: &parser::Stmt) -> Result<(), InterpreterError> {
        match statement {
            Stmt::Expression { .. } => Ok(()),
            Stmt::Print { expression } => {
                let value = self.interpret_expression(expression)?;
                println!("{}", value);
                Ok(())
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
            parser::LoxParserValue::Number(value) => Ok(LoxValue::Number(*value)),
            parser::LoxParserValue::String(value) => Ok(LoxValue::String(value.clone())),
            parser::LoxParserValue::Boolean(value) => Ok(LoxValue::Boolean(*value)),
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

        // Return nil if one of the operands is nil
        if let (LoxValue::Nil, _) | (_, LoxValue::Nil) = (left.clone(), right.clone()) {
            return Ok(LoxValue::Nil);
        }

        match operator {
            parser::LoxBinaryOperator::Plus => match left {
                LoxValue::Number(left) => match right {
                    LoxValue::Number(right) => Ok(LoxValue::Number(left + right)),
                    _ => Err(InterpreterError::OperandsDoNotMatch),
                },
                LoxValue::String(left) => match right {
                    LoxValue::String(right) => Ok(LoxValue::String(left + &right)),
                    _ => Err(InterpreterError::OperandsDoNotMatch),
                },
                _ => Err(InterpreterError::OperandsMustBeNumbers),
            },
            parser::LoxBinaryOperator::Minus => {
                if let (LoxValue::Number(left), LoxValue::Number(right)) = (left, right) {
                    Ok(LoxValue::Number(left - right))
                } else {
                    Err(InterpreterError::OperandsMustBeNumbers)
                }
            }
            parser::LoxBinaryOperator::Star => {
                if let (LoxValue::Number(left), LoxValue::Number(right)) = (left, right) {
                    Ok(LoxValue::Number(left * right))
                } else {
                    Err(InterpreterError::OperandsMustBeNumbers)
                }
            }
            parser::LoxBinaryOperator::Slash => {
                if let (LoxValue::Number(left), LoxValue::Number(right)) = (left, right) {
                    // Handle division by zero by returning a nil value
                    let result = left / right;
                    if result.is_nan() || result.is_infinite() {
                        Ok(LoxValue::Nil)
                    } else {
                        Ok(LoxValue::Number(result))
                    }
                } else {
                    Err(InterpreterError::OperandsMustBeNumbers)
                }
            }
            parser::LoxBinaryOperator::Greater => {
                if let (LoxValue::Number(left), LoxValue::Number(right)) = (left, right) {
                    Ok(LoxValue::Boolean(left > right))
                } else {
                    Err(InterpreterError::OperandsMustBeNumbers)
                }
            }
            parser::LoxBinaryOperator::GreaterEqual => {
                if let (LoxValue::Number(left), LoxValue::Number(right)) = (left, right) {
                    Ok(LoxValue::Boolean(left >= right))
                } else {
                    Err(InterpreterError::OperandsMustBeNumbers)
                }
            }
            parser::LoxBinaryOperator::Less => {
                if let (LoxValue::Number(left), LoxValue::Number(right)) = (left, right) {
                    Ok(LoxValue::Boolean(left < right))
                } else {
                    Err(InterpreterError::OperandsMustBeNumbers)
                }
            }
            parser::LoxBinaryOperator::LessEqual => {
                if let (LoxValue::Number(left), LoxValue::Number(right)) = (left, right) {
                    Ok(LoxValue::Boolean(left <= right))
                } else {
                    Err(InterpreterError::OperandsMustBeNumbers)
                }
            }
            parser::LoxBinaryOperator::BangEqual => Ok(LoxValue::Boolean(left != right)),
            parser::LoxBinaryOperator::EqualEqual => Ok(LoxValue::Boolean(left == right)),
            parser::LoxBinaryOperator::And => {
                Ok(LoxValue::Boolean(left.is_truthy() && right.is_truthy()))
            }
            parser::LoxBinaryOperator::Or => {
                Ok(LoxValue::Boolean(left.is_truthy() || right.is_truthy()))
            }
        }
    }

    fn interpret_unary(
        &self,
        operator: &parser::LoxUnaryOperator,
        right: &parser::Expr,
    ) -> Result<LoxValue, InterpreterError> {
        let right = self.interpret_expression(right)?;

        if let LoxValue::Nil = right {
            return Ok(LoxValue::Nil);
        }

        match operator {
            parser::LoxUnaryOperator::Minus => {
                if let LoxValue::Number(right) = right {
                    Ok(LoxValue::Number(-right))
                } else {
                    Err(InterpreterError::OperandMustBeNumber)
                }
            }
            parser::LoxUnaryOperator::Bang => Ok(LoxValue::Boolean(!right.is_truthy())),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum LoxValue {
    Number(OrderedFloat<f64>),
    String(String),
    Boolean(bool),
    Nil,
}

impl std::fmt::Display for LoxValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxValue::Number(value) => write!(f, "{value}"),
            LoxValue::String(value) => write!(f, "{value}"),
            LoxValue::Boolean(value) => write!(f, "{value}"),
            LoxValue::Nil => write!(f, "nil"),
        }
    }
}

impl LoxValue {
    fn is_truthy(&self) -> bool {
        match self {
            LoxValue::Nil => false,
            LoxValue::Boolean(value) => *value,
            LoxValue::Number(value) => !value.is_nan() && !value.is_infinite() && *value != 0.0,
            _ => true,
        }
    }
}
