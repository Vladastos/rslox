//! TODO:
//!  - Change the return type of the `name()` method of `LoxValue` to `&str`.

use log::debug;
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
        debug!(
            "Interpreting binary expression: {} {} {}",
            left, operator, right
        );
        // Return nil if one of the operands is nil
        if let (LoxValue::Nil, _) | (_, LoxValue::Nil) = (left.clone(), right.clone()) {
            return Ok(LoxValue::Nil);
        }

        match operator {
            parser::LoxBinaryOperator::Plus => match left {
                LoxValue::Number(left) => match right {
                    LoxValue::Number(right) => Ok(LoxValue::Number(left + right)),
                    _ => Err(InterpreterError::InvalidOperandType {
                        found: right.name(),
                        expected: "number",
                    }),
                },
                LoxValue::String(left) => match right {
                    LoxValue::String(right) => Ok(LoxValue::String(left + &right)),
                    _ => Err(InterpreterError::InvalidOperandType {
                        found: right.name(),
                        expected: "string",
                    }),
                },
                _ => Err(InterpreterError::InvalidOperandType {
                    found: left.name(),
                    expected: "number or string",
                }),
            },
            parser::LoxBinaryOperator::Minus => match left {
                LoxValue::Number(left) => match right {
                    LoxValue::Number(right) => Ok(LoxValue::Number(left - right)),
                    _ => Err(InterpreterError::InvalidOperandType {
                        found: right.name(),
                        expected: "number",
                    }),
                },
                _ => Err(InterpreterError::InvalidOperandType {
                    found: left.name(),
                    expected: "number",
                }),
            },
            parser::LoxBinaryOperator::Star => match left {
                LoxValue::Number(left) => match right {
                    LoxValue::Number(right) => Ok(LoxValue::Number(left * right)),
                    _ => Err(InterpreterError::InvalidOperandType {
                        found: right.name(),
                        expected: "number",
                    }),
                },
                _ => Err(InterpreterError::InvalidOperandType {
                    found: left.name(),
                    expected: "number",
                }),
            },
            parser::LoxBinaryOperator::Slash => match left {
                LoxValue::Number(left) => match right {
                    LoxValue::Number(right) => Ok(LoxValue::Number(left / right)),
                    _ => Err(InterpreterError::InvalidOperandType {
                        found: right.name(),
                        expected: "number",
                    }),
                },
                _ => Err(InterpreterError::InvalidOperandType {
                    found: left.name(),
                    expected: "number",
                }),
            },
            parser::LoxBinaryOperator::Greater => match left {
                LoxValue::Number(left) => match right {
                    LoxValue::Number(right) => Ok(LoxValue::Boolean(left > right)),
                    _ => Err(InterpreterError::InvalidOperandType {
                        found: right.name(),
                        expected: "number",
                    }),
                },
                _ => Err(InterpreterError::InvalidOperandType {
                    found: left.name(),
                    expected: "number",
                }),
            },
            parser::LoxBinaryOperator::GreaterEqual => match left {
                LoxValue::Number(left) => match right {
                    LoxValue::Number(right) => Ok(LoxValue::Boolean(left >= right)),
                    _ => Err(InterpreterError::InvalidOperandType {
                        found: right.name(),
                        expected: "number",
                    }),
                },
                _ => Err(InterpreterError::InvalidOperandType {
                    found: left.name(),
                    expected: "number",
                }),
            },
            parser::LoxBinaryOperator::Less => match left {
                LoxValue::Number(left) => match right {
                    LoxValue::Number(right) => Ok(LoxValue::Boolean(left < right)),
                    _ => Err(InterpreterError::InvalidOperandType {
                        found: right.name(),
                        expected: "number",
                    }),
                },
                _ => Err(InterpreterError::InvalidOperandType {
                    found: left.name(),
                    expected: "number",
                }),
            },
            parser::LoxBinaryOperator::LessEqual => match left {
                LoxValue::Number(left) => match right {
                    LoxValue::Number(right) => Ok(LoxValue::Boolean(left <= right)),
                    _ => Err(InterpreterError::InvalidOperandType {
                        found: right.name(),
                        expected: "number",
                    }),
                },
                _ => Err(InterpreterError::InvalidOperandType {
                    found: left.name(),
                    expected: "number",
                }),
            },
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
                    Err(InterpreterError::InvalidOperandType {
                        found: right.name(),
                        expected: "number",
                    })
                }
            }
            parser::LoxUnaryOperator::Bang => Ok(LoxValue::Boolean(!right.is_truthy())),
        }
    }
}

/// The internal representation of a Lox value.

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LoxValue {
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

    fn name(&self) -> String {
        match self {
            LoxValue::Number(_) => "number".to_string(),
            LoxValue::String(_) => "string".to_string(),
            LoxValue::Boolean(_) => "boolean".to_string(),
            LoxValue::Nil => "nil".to_string(),
        }
    }
}
