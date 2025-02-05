//! TODO:
//!  - Change the return type of the `name()` method of `LoxValue` to `&str`.

use std::collections::HashMap;

use log::debug;
use ordered_float::OrderedFloat;

use super::InterpreterError;
use crate::rslox::parser;
use crate::rslox::parser::{Expr, Stmt};

pub struct Interpreter<'a> {
    environment: &'a mut Environment,
}

impl Interpreter<'_> {
    pub fn new(environment: &mut Environment) -> Interpreter {
        Interpreter { environment }
    }

    pub fn run(&mut self, statements: &[parser::Stmt]) -> Result<(), InterpreterError> {
        for statement in statements {
            self.interpret_statement(statement)?
        }
        Ok(())
    }

    fn interpret_statement(&mut self, statement: &parser::Stmt) -> Result<(), InterpreterError> {
        match statement {
            Stmt::Expression { expression } => {
                self.interpret_expression(expression)?;
                Ok(())
            }
            Stmt::Print { expression } => {
                let value = self.interpret_expression(expression)?;
                println!("{}", value);
                Ok(())
            }
            Stmt::VarDeclaration { name, initializer } => self.interpret_variable_declaration(
                name,
                initializer.as_ref().map(|expr| expr.clone()),
            ),
            Stmt::Block { statements } => {
                self.environment.new_scope();
                self.run(statements)?;
                self.environment.restore_scope();
                Ok(())
            }
        }
    }

    fn interpret_expression(
        &mut self,
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
            Expr::Variable { name } => self.interpret_variable(name),
            Expr::Assignment { name, value } => {
                let value = self.interpret_expression(value)?;
                let value = self.environment.assign(name, value.clone());
                if let Some(value) = value {
                    Ok(value)
                } else {
                    Err(InterpreterError::UndefinedVariable {
                        name: name.to_owned(),
                    })
                }
            }
        }
    }

    fn interpret_variable_declaration(
        &mut self,
        name: &str,
        initializer: Option<parser::Expr>,
    ) -> Result<(), InterpreterError> {
        let value = if let Some(initializer) = initializer {
            self.interpret_expression(&initializer)?
        } else {
            LoxValue::Nil
        };
        self.environment.define(name.to_owned(), value);
        Ok(())
    }

    fn interpret_variable(&self, name: &str) -> Result<LoxValue, InterpreterError> {
        self.environment
            .get(name)
            .ok_or_else(|| InterpreterError::UndefinedVariable {
                name: name.to_owned(),
            })
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
        &mut self,
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
                if left.is_truthy() {
                    Ok(right)
                } else {
                    Ok(LoxValue::Boolean(left.is_truthy()))
                }
            }
            parser::LoxBinaryOperator::Or => {
                if !left.is_truthy() {
                    Ok(right)
                } else {
                    Ok(LoxValue::Boolean(left.is_truthy()))
                }
            }
        }
    }

    fn interpret_unary(
        &mut self,
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

/// An environment is a mapping from variable names to values.
#[derive(Debug, Clone)]
pub struct Environment {
    parent: Option<Box<Environment>>,
    values: HashMap<String, LoxValue>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            parent: None,
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: LoxValue) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<LoxValue> {
        match self.values.get(name) {
            Some(value) => Some(value.clone()),
            None => match &self.parent {
                Some(parent) => parent.get(name),
                None => None,
            },
        }
    }

    pub fn assign(&mut self, name: &str, new_value: LoxValue) -> Option<LoxValue> {
        match self.values.get_mut(name) {
            Some(value) => Some(std::mem::replace(value, new_value.clone())),
            None => match &mut self.parent {
                Some(parent) => parent.assign(name, new_value),
                None => None,
            },
        }
    }

    pub fn new_scope(&mut self) {
        self.parent = Some(Box::new(self.clone()));
        self.values = HashMap::new();
    }
    pub fn restore_scope(&mut self) {
        assert!(self.parent.is_some());
        let parent = self.parent.take().unwrap();
        self.values = parent.values;
        self.parent = parent.parent;
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
