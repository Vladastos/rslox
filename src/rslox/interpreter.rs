//! TODO:
//!  - Change the return type of the `name()` method of `LoxValue` to `&str`.

use crate::rslox::builtins::BUILTINS;

use std::collections::HashMap;

use ordered_float::OrderedFloat;

use super::parser::FunctionParam;
use super::InterpreterError;
use crate::rslox::parser;
use crate::rslox::parser::{Expr, Stmt};

/// Interpreter

pub struct Interpreter<'a> {
    environment: &'a mut Environment,
}

impl Interpreter<'_> {
    pub fn new(environment: &mut Environment) -> Interpreter {
        Interpreter { environment }
    }

    /// Executes a list of statements in the current environment.
    ///
    /// This function iterates over each statement in the provided slice,
    /// interpreting it within the current environment. If interpreting
    /// any statement results in an error, the function returns an
    /// `InterpreterError`.
    ///
    /// # Arguments
    ///
    /// * `statements` - A slice of statements to be executed.
    ///
    /// # Returns
    ///
    /// * `Result<(), InterpreterError>` - Returns `Ok(())` if all statements
    /// are successfully executed, otherwise returns an `InterpreterError`.
    pub fn run(&mut self, statements: &[parser::Stmt]) -> Result<(), InterpreterError> {
        for statement in statements {
            self.interpret_statement(statement)?
        }
        Ok(())
    }

    /// Executes a statement in the current environment.
    ///
    /// This function interprets a given statement in the current environment.
    /// If interpreting the statement results in an error, the function returns an
    /// `InterpreterError`.
    ///
    /// # Arguments
    ///
    /// * `statement` - The statement to be executed.
    ///
    /// # Returns
    ///
    /// * `Result<(), InterpreterError>` - Returns `Ok(())` if the statement is
    /// successfully executed, otherwise returns an `InterpreterError`.
    /// * Err(InterpreterError::Return { value }) - Returns an `InterpreterError::Return`
    /// with the value that was returned from the function.
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
            Stmt::MutDeclaration { name, initializer } => self.interpret_variable_declaration(
                name,
                initializer.as_ref().map(|expr| expr.clone()),
            ),
            Stmt::ConstDeclaration { name, initializer } => {
                self.interpret_constant_declaration(name, Some(initializer.clone()))
            }
            Stmt::Function { name, params, body } => {
                self.interpret_function_declaration(name, params, body.clone())?;
                Ok(())
            }
            Stmt::Block { statements } => {
                self.environment.new_scope();
                self.run(statements)?;
                self.environment.restore_scope();
                Ok(())
            }
            Stmt::FunctionBlock { statements } => {
                self.run(statements)?;
                Ok(())
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => self.interpret_if_statement(condition, then_branch, else_branch),
            Stmt::While { condition, body } => self.interpret_while_statement(condition, body),
            Stmt::Return { value } => {
                let value = if let Some(value) = value {
                    self.interpret_expression(value)?
                } else {
                    LoxValue::Nil
                };
                Err(InterpreterError::Return { value })
            }
        }
    }

    /// Interprets an if statement.
    ///
    /// Evaluates the condition expression.
    /// If the result is truthy, it interprets the then-branch statement.
    /// Otherwise, it interprets the else-branch statement if present, or returns Ok(()) if not.
    fn interpret_if_statement(
        &mut self,
        condition: &parser::Expr,
        then_branch: &parser::Stmt,
        else_branch: &Option<Box<parser::Stmt>>,
    ) -> Result<(), InterpreterError> {
        let condition_result = self.interpret_expression(condition)?;
        if condition_result.is_truthy() {
            self.interpret_statement(then_branch)
        } else {
            if let Some(else_branch) = else_branch {
                self.interpret_statement(else_branch)
            } else {
                Ok(())
            }
        }
    }

    /// Interprets a while statement.
    ///
    /// Evaluates the condition expression until it is no longer truthy.
    /// While the condition is truthy, it interprets the body statement.
    /// Returns an error if interpreting the condition or body results in an error.
    fn interpret_while_statement(
        &mut self,
        condition: &parser::Expr,
        body: &parser::Stmt,
    ) -> Result<(), InterpreterError> {
        while self.interpret_expression(condition)?.is_truthy() {
            self.interpret_statement(body)?
        }
        Ok(())
    }

    /// Evaluates an expression and returns the result.
    ///
    /// This function interprets a given expression and returns the result.
    /// If interpreting the expression results in an error, the function returns an
    /// `InterpreterError`.
    ///
    /// # Arguments
    ///
    /// * `expression` - The expression to be evaluated.
    ///
    /// # Returns
    ///
    /// * `Result<LoxValue, InterpreterError>` - Returns `Ok(LoxValue)` if the expression is
    /// successfully evaluated, otherwise returns an `InterpreterError`.
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
                let value = self.environment.assign(name, value.clone())?;
                Ok(value)
            }
            Expr::Call { callee, arguments } => return self.interpret_call(callee, arguments),
        }
    }

    /// defines a variable in the current environment.
    ///
    /// If the variable declaration has an initializer, this function evaluates the initializer
    /// expression and assigns the result to the variable.
    /// Otherwise, the variable is assigned the value `LoxValue::Nil`.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the variable to be declared.
    /// * `initializer` - An optional expression to be evaluated and assigned to the variable.
    ///
    /// # Returns
    ///
    /// * `Result<(), InterpreterError>` - Returns `Ok(())` if the variable is successfully
    /// declared, otherwise returns an `InterpreterError`.
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
        self.environment.define_mutable(name.to_owned(), value);
        Ok(())
    }

    fn interpret_constant_declaration(
        &mut self,
        name: &str,
        initializer: Option<parser::Expr>,
    ) -> Result<(), InterpreterError> {
        let value = self.interpret_expression(&initializer.unwrap())?;

        self.environment.define_constant(name.to_owned(), value);

        Ok(())
    }

    /// Interprets a function declaration and defines a ClojureFunction function in the current environment.
    ///
    /// This function creates a new ClojureFunction `LoxValue` from the given function name, parameters,
    /// and body. It then defines this function in the current environment, making it available
    /// for invocation in the interpreted Lox code.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the function to be declared.
    /// * `parameters` - A slice of parameter names for the function.
    /// * `body` - The body of the function as a statement block.
    ///
    /// # Returns
    ///
    /// * `Result<(), InterpreterError>` - Returns `Ok(())` if the function is successfully
    /// declared, otherwise returns an `InterpreterError`.
    fn interpret_function_declaration(
        &mut self,
        name: &str,
        parameters: &[FunctionParam],
        body: Box<parser::Stmt>,
    ) -> Result<(), InterpreterError> {
        //! TODO: Move the captured environment into the function
        //! Ideally the function should only hold a reference to the captured environment, but
        //! that would require a lot of refactoring

        let function = LoxValue::ClojureFunction {
            name: name.to_owned(),
            parameters: parameters.to_vec(),
            body: Some(body),
        };
        self.environment.define_constant(name.to_owned(), function);
        Ok(())
    }

    /// Looks up the value of a variable in the current environment.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the variable to look up.
    ///
    /// # Returns
    ///
    /// * `Result<LoxValue, InterpreterError>` - Returns `Ok(LoxValue)` if the variable is
    /// defined, otherwise returns an `InterpreterError::UndefinedVariable`.
    fn interpret_variable(&self, name: &str) -> Result<LoxValue, InterpreterError> {
        self.environment.get(name, &mut false)
    }
    /// Converts a LoxParserValue to a LoxValue.
    ///
    /// This function takes a LoxParserValue and returns its corresponding LoxValue.
    /// If the LoxParserValue is not a valid LoxValue, it returns an InterpreterError.
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

    /// Evaluates a binary expression and returns the result.
    ///
    /// This function first evaluates the left and right sides of the expression and then
    /// applies the binary operator to the results. If the binary operator does not
    /// support the types of the left and right sides, it returns an `InterpreterError`.
    ///
    /// # Arguments
    ///
    /// * `left` - The left side of the binary expression.
    /// * `operator` - The binary operator to apply to the left and right sides.
    /// * `right` - The right side of the binary expression.
    ///
    /// # Returns
    ///
    /// * `Result<LoxValue, InterpreterError>` - Returns `Ok(LoxValue)` if the binary expression
    /// is successfully evaluated, otherwise returns an `InterpreterError`.
    fn interpret_binary(
        &mut self,
        left: &parser::Expr,
        operator: &parser::LoxBinaryOperator,
        right: &parser::Expr,
    ) -> Result<LoxValue, InterpreterError> {
        let left = self.interpret_expression(left)?;
        let right = self.interpret_expression(right)?;

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
                    LoxValue::Number(right) => {
                        let value = left / right;
                        if value.is_infinite() {
                            return Err(InterpreterError::DivisionByZero);
                        }
                        Ok(LoxValue::Number(value))
                    }
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
                    Ok(left)
                }
            }
            parser::LoxBinaryOperator::Or => {
                if !left.is_truthy() {
                    Ok(right)
                } else {
                    Ok(left)
                }
            }
        }
    }

    /// Evaluates a unary expression and returns the result.
    ///
    /// This function interprets a given unary expression and returns the result.
    /// If interpreting the expression results in an error, the function returns an
    /// `InterpreterError`.
    ///
    /// # Arguments
    ///
    /// * `operator` - The unary operator to be evaluated.
    /// * `right` - The expression to the right of the operator.
    ///
    /// # Returns
    ///
    /// * `Result<LoxValue, InterpreterError>` - Returns `Ok(LoxValue)` if the expression is
    /// successfully evaluated, otherwise returns an `InterpreterError`.
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

    /// Interprets a call expression and returns the result.
    ///
    /// This function interprets a given call expression and returns the result.
    /// If interpreting the expression results in an error, the function returns an
    /// `InterpreterError`.
    ///
    /// # Arguments
    ///
    /// * `callee` - The expression to be called.
    /// * `arguments` - A slice of expressions to be passed as arguments to the function.
    ///
    /// # Returns
    ///
    /// * `Result<LoxValue, InterpreterError>` - Returns `Ok(LoxValue)` if the expression is
    /// successfully evaluated, otherwise returns an `InterpreterError`.
    fn interpret_call(
        &mut self,
        callee: &parser::Expr,
        arguments: &[parser::Expr],
    ) -> Result<LoxValue, InterpreterError> {
        let callee = self.interpret_expression(callee)?;

        let arguments = arguments
            .iter()
            .map(|argument| self.interpret_expression(argument))
            .collect::<Result<Vec<LoxValue>, InterpreterError>>()?;

        return callee.call(self, &arguments);
    }
}

/// An environment is a mapping from variable names to values.
#[derive(Debug, Clone)]
pub struct Environment {
    parent: Option<Box<Environment>>,
    values: HashMap<String, LoxValueType>,
    is_function: bool,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            parent: None,
            values: (*BUILTINS).clone(),
            is_function: false,
        }
    }

    pub fn define_constant(&mut self, name: String, value: LoxValue) {
        self.values.insert(name, LoxValueType::Constant(value));
    }

    pub fn define_mutable(&mut self, name: String, value: LoxValue) {
        self.values.insert(name, LoxValueType::Mutable(value));
    }

    /// This method looks up a variable in the current environment and its parent environments.:
    /// 1. Once it leaves a function scope, it sets a flag to true.
    /// 2. If the variable is found after leaving a function scope, it checks if the variable is mutable:
    ///     - If the variable is mutable, it returns an error.
    ///     - If the variable is not mutable, it returns the value.
    pub fn get(&self, name: &str, left_scope: &mut bool) -> Result<LoxValue, InterpreterError> {
        match self.values.get(name) {
            Some(value) => match value {
                LoxValueType::Mutable(value) => {
                    if *left_scope {
                        return Err(InterpreterError::MutableVariableOutsideOfFunction {
                            name: name.to_string(),
                        });
                    }
                    Ok(value.clone())
                }
                LoxValueType::Constant(value) => Ok(value.clone()),
            },
            None => {
                if self.is_function {
                    *left_scope = true;
                }
                return match &self.parent {
                    Some(parent) => parent.get(name, left_scope),
                    None => Err(InterpreterError::UndefinedVariable {
                        name: name.to_string(),
                    }),
                };
            }
        }
    }

    pub fn assign(
        &mut self,
        name: &str,
        new_value: LoxValue,
    ) -> Result<LoxValue, InterpreterError> {
        match self.values.get_mut(name) {
            Some(value) => match value {
                LoxValueType::Constant(_) => Err(InterpreterError::CannotAssingnToConstant {
                    name: name.to_string(),
                }),
                LoxValueType::Mutable(value) => {
                    *value = new_value.clone();
                    Ok(new_value)
                }
            },
            None => match &mut self.parent {
                Some(parent) => parent.assign(name, new_value),
                None => Err(InterpreterError::UndefinedVariable {
                    name: name.to_string(),
                }),
            },
        }
    }

    pub fn new_scope(&mut self) {
        self.parent = Some(Box::new(self.clone()));
        self.values = HashMap::new();
        self.is_function = false;
    }

    pub fn new_function_scope(&mut self) {
        self.parent = Some(Box::new(self.clone()));
        self.values = HashMap::new();
        self.is_function = true;
    }
    pub fn restore_scope(&mut self) {
        assert!(self.parent.is_some());
        let parent = self.parent.take().unwrap();
        self.values = parent.values;
        self.parent = parent.parent;
    }
}

/// The type of a Lox value.
/// Either a constant value or a mutable value.
#[derive(Debug, Clone)]
pub enum LoxValueType {
    Constant(LoxValue),
    Mutable(LoxValue),
}

/// The internal representation of a Lox value.
#[derive(Debug, Clone)]
pub enum LoxValue {
    Number(OrderedFloat<f64>),
    String(String),
    Boolean(bool),
    ClojureFunction {
        name: String,
        body: Option<Box<parser::Stmt>>,
        parameters: Vec<FunctionParam>,
    },
    BuiltinFunction {
        name: String,
        parameters: Vec<String>,
        function: fn(&mut Interpreter, &[LoxValue]) -> Result<LoxValue, InterpreterError>,
    },
    Nil,
}

impl PartialEq for LoxValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LoxValue::Number(left), LoxValue::Number(right)) => left == right,
            (LoxValue::String(left), LoxValue::String(right)) => left == right,
            (LoxValue::Boolean(left), LoxValue::Boolean(right)) => left == right,
            (LoxValue::Nil, LoxValue::Nil) => true,
            _ => false,
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
            LoxValue::ClojureFunction { .. } => "function".to_string(),
            LoxValue::BuiltinFunction { .. } => "function".to_string(),
            LoxValue::Nil => "nil".to_string(),
        }
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: &[LoxValue],
    ) -> Result<LoxValue, InterpreterError> {
        return match self {
            LoxValue::ClojureFunction {
                body, parameters, ..
            } => {
                if arguments.len() != parameters.len() {
                    return Err(InterpreterError::InvalidArgumentCount {
                        expected: parameters.len(),
                        found: arguments.len(),
                    });
                }

                interpreter.environment.new_function_scope();
                for (parameter, argument) in parameters.iter().zip(arguments.iter()) {
                    match parameter {
                        FunctionParam::Mut { name } => {
                            interpreter
                                .environment
                                .define_mutable(name.clone(), argument.clone());
                        }
                        FunctionParam::Const { name } => {
                            interpreter
                                .environment
                                .define_constant(name.clone(), argument.clone());
                        }
                    }
                }

                let interpreter_result = interpreter.interpret_statement(&body.clone().unwrap());

                let return_value: Result<LoxValue, InterpreterError> = match interpreter_result {
                    Ok(_value) => Ok(LoxValue::Nil),
                    Err(InterpreterError::Return { value }) => Ok(value),
                    Err(error) => Err(error),
                };

                interpreter.environment.restore_scope();
                return_value
            }
            LoxValue::BuiltinFunction {
                parameters,
                function,
                ..
            } => {
                if arguments.len() != parameters.len() {
                    return Err(InterpreterError::InvalidArgumentCount {
                        expected: parameters.len(),
                        found: arguments.len(),
                    });
                }

                function(interpreter, arguments)
            }
            _ => Err(InterpreterError::NonFunctionCall { name: self.name() }),
        };
    }
}

impl std::fmt::Display for LoxValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxValue::Number(value) => write!(f, "{}", value),
            LoxValue::String(value) => write!(f, "{}", value),
            LoxValue::Boolean(value) => write!(f, "{}", value),
            LoxValue::ClojureFunction { name, .. } => write!(f, "<fn {}>", name),
            LoxValue::BuiltinFunction { name, .. } => write!(f, "<fn {}>", name),
            LoxValue::Nil => write!(f, "nil"),
        }
    }
}
