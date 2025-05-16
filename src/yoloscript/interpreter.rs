//! TODO:
//!  - Change the return type of the `name()` method of `YoloValue` to `&str`.

use log::debug;
use ordered_float::OrderedFloat;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use super::builtins::init_builtins;
use super::InterpreterError;
use crate::yoloscript::capturer::Capturer;
use crate::yoloscript::parser;
use crate::yoloscript::parser::{Expr, Stmt};

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
        debug!("Running {} statements", statements.len());
        debug!("Statements: {:#?}", statements);
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
                let result = self.run(statements);
                self.environment.restore_scope();
                result
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
                    YoloValue::Nil
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
    /// * `Result<YoloValue, InterpreterError>` - Returns `Ok(YoloValue)` if the expression is
    /// successfully evaluated, otherwise returns an `InterpreterError`.
    fn interpret_expression(
        &mut self,
        expression: &parser::Expr,
    ) -> Result<YoloValue, InterpreterError> {
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
    /// Otherwise, the variable is assigned the value `YoloValue::Nil`.
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
            YoloValue::Nil
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
    /// This function creates a new ClojureFunction `YoloValue` from the given function name, parameters,
    /// and body. It then defines this function in the current environment, making it available
    /// for invocation in the interpreted Yolo code.
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
        parameters: &[String],
        body: Box<parser::Stmt>,
    ) -> Result<(), InterpreterError> {
        let function = YoloValue::ClojureFunction {
            name: name.to_owned(),
            captured_variables: self.capture_variables_in_function_body(&body)?,
            parameters: parameters.to_vec(),
            body: Some(body),
        };
        self.environment.define_constant(name.to_owned(), function);
        Ok(())
    }

    /// Captures variables in a function body and returns a map of variable names to their values.
    fn capture_variables_in_function_body(
        &mut self,
        body: &parser::Stmt,
    ) -> Result<HashMap<String, Rc<RefCell<YoloValueType>>>, InterpreterError> {
        Capturer::new(self.environment).capture(body)
    }

    /// Looks up the value of a variable in the current environment.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the variable to look up.
    ///
    /// # Returns
    ///
    /// * `Result<YoloValue, InterpreterError>` - Returns `Ok(YoloValue)` if the variable is
    /// defined, otherwise returns an `InterpreterError::UndefinedVariable`.
    fn interpret_variable(&self, name: &str) -> Result<YoloValue, InterpreterError> {
        self.environment
            .get(name)
            .ok_or_else(|| InterpreterError::UndefinedVariable {
                name: name.to_owned(),
            })
    }
    /// Converts a YoloParserValue to a YoloValue.
    ///
    /// This function takes a YoloParserValue and returns its corresponding YoloValue.
    /// If the YoloParserValue is not a valid YoloValue, it returns an InterpreterError.
    fn interpret_literal(
        &self,
        literal: &parser::YoloParserValue,
    ) -> Result<YoloValue, InterpreterError> {
        match literal {
            parser::YoloParserValue::Number(value) => Ok(YoloValue::Number(*value)),
            parser::YoloParserValue::String(value) => Ok(YoloValue::String(value.clone())),
            parser::YoloParserValue::Boolean(value) => Ok(YoloValue::Boolean(*value)),
            parser::YoloParserValue::Nil => Ok(YoloValue::Nil),
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
    /// * `Result<YoloValue, InterpreterError>` - Returns `Ok(YoloValue)` if the binary expression
    /// is successfully evaluated, otherwise returns an `InterpreterError`.
    fn interpret_binary(
        &mut self,
        left: &parser::Expr,
        operator: &parser::YoloBinaryOperator,
        right: &parser::Expr,
    ) -> Result<YoloValue, InterpreterError> {
        let left = self.interpret_expression(left)?;
        let right = self.interpret_expression(right)?;

        match operator {
            // TODO: Instead of allowing type coercion, we should add a better print function to the standard library
            parser::YoloBinaryOperator::Plus => match left {
                YoloValue::Number(left) => match right {
                    YoloValue::Number(right) => Ok(YoloValue::Number(left + right)),
                    YoloValue::String(right) => Ok(YoloValue::String(left.to_string() + &right)),
                    _ => Err(InterpreterError::InvalidOperandType {
                        found: right.name(),
                        expected: "number or string",
                    }),
                },
                YoloValue::String(left) => match right {
                    YoloValue::String(right) => Ok(YoloValue::String(left + &right)),
                    YoloValue::Number(right) => {
                        Ok(YoloValue::String(left.to_string() + &right.to_string()))
                    }
                    _ => Err(InterpreterError::InvalidOperandType {
                        found: right.name(),
                        expected: "number or string",
                    }),
                },
                _ => Err(InterpreterError::InvalidOperandType {
                    found: left.name(),
                    expected: "number or string",
                }),
            },
            parser::YoloBinaryOperator::Minus => match left {
                YoloValue::Number(left) => match right {
                    YoloValue::Number(right) => Ok(YoloValue::Number(left - right)),
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
            parser::YoloBinaryOperator::Star => match left {
                YoloValue::Number(left) => match right {
                    YoloValue::Number(right) => Ok(YoloValue::Number(left * right)),
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
            parser::YoloBinaryOperator::Slash => match left {
                YoloValue::Number(left) => match right {
                    YoloValue::Number(right) => {
                        let value = left / right;
                        if value.is_infinite() {
                            return Err(InterpreterError::DivisionByZero);
                        }
                        Ok(YoloValue::Number(value))
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
            parser::YoloBinaryOperator::Greater => match left {
                YoloValue::Number(left) => match right {
                    YoloValue::Number(right) => Ok(YoloValue::Boolean(left > right)),
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
            parser::YoloBinaryOperator::GreaterEqual => match left {
                YoloValue::Number(left) => match right {
                    YoloValue::Number(right) => Ok(YoloValue::Boolean(left >= right)),
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
            parser::YoloBinaryOperator::Less => match left {
                YoloValue::Number(left) => match right {
                    YoloValue::Number(right) => Ok(YoloValue::Boolean(left < right)),
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
            parser::YoloBinaryOperator::LessEqual => match left {
                YoloValue::Number(left) => match right {
                    YoloValue::Number(right) => Ok(YoloValue::Boolean(left <= right)),
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
            parser::YoloBinaryOperator::BangEqual => Ok(YoloValue::Boolean(left != right)),
            parser::YoloBinaryOperator::EqualEqual => Ok(YoloValue::Boolean(left == right)),
            parser::YoloBinaryOperator::And => {
                if left.is_truthy() {
                    Ok(right)
                } else {
                    Ok(left)
                }
            }
            parser::YoloBinaryOperator::Or => {
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
    /// * `Result<YoloValue, InterpreterError>` - Returns `Ok(YoloValue)` if the expression is
    /// successfully evaluated, otherwise returns an `InterpreterError`.
    fn interpret_unary(
        &mut self,
        operator: &parser::YoloUnaryOperator,
        right: &parser::Expr,
    ) -> Result<YoloValue, InterpreterError> {
        let right = self.interpret_expression(right)?;

        if let YoloValue::Nil = right {
            return Ok(YoloValue::Nil);
        }

        match operator {
            parser::YoloUnaryOperator::Minus => {
                if let YoloValue::Number(right) = right {
                    Ok(YoloValue::Number(-right))
                } else {
                    Err(InterpreterError::InvalidOperandType {
                        found: right.name(),
                        expected: "number",
                    })
                }
            }
            parser::YoloUnaryOperator::Bang => Ok(YoloValue::Boolean(!right.is_truthy())),
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
    /// * `Result<YoloValue, InterpreterError>` - Returns `Ok(YoloValue)` if the expression is
    /// successfully evaluated, otherwise returns an `InterpreterError`.
    fn interpret_call(
        &mut self,
        callee: &parser::Expr,
        arguments: &[parser::Expr],
    ) -> Result<YoloValue, InterpreterError> {
        let callee = self.interpret_expression(callee)?;

        let arguments = arguments
            .iter()
            .map(|argument| self.interpret_expression(argument))
            .collect::<Result<Vec<YoloValue>, InterpreterError>>()?;

        return callee.call(self, &arguments);
    }
}

/// An environment is a mapping from variable names to values.
#[derive(Debug, Clone)]
pub struct Environment {
    parent: Option<Box<Environment>>,
    values: HashMap<String, Rc<RefCell<YoloValueType>>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            parent: None,
            values: init_builtins(),
        }
    }

    pub fn define_constant(&mut self, name: String, value: YoloValue) {
        self.values
            .insert(name, Rc::new(RefCell::new(YoloValueType::Constant(value))));
    }

    /// This allows us to insert a constant into the environment by reference
    pub fn inject_variable(&mut self, name: String, value: &Rc<RefCell<YoloValueType>>) {
        self.values.insert(name, value.clone());
    }

    pub fn extract_variable(&self, name: &str) -> Option<Rc<RefCell<YoloValueType>>> {
        self.values.get(name).cloned()
    }
    pub fn define_mutable(&mut self, name: String, value: YoloValue) {
        self.values
            .insert(name, Rc::new(RefCell::new(YoloValueType::Mutable(value))));
    }

    pub fn get(&self, name: &str) -> Option<YoloValue> {
        match self.values.get(name) {
            Some(value) => Some(match value.as_ref().borrow().deref() {
                YoloValueType::Constant(value) => value.clone(),
                YoloValueType::Mutable(value) => value.clone(),
            }),
            None => match &self.parent {
                Some(parent) => parent.get(name),
                None => None,
            },
        }
    }

    pub fn assign(
        &mut self,
        name: &str,
        new_value: YoloValue,
    ) -> Result<YoloValue, InterpreterError> {
        match self.values.get_mut(name) {
            Some(value) => match value.as_ref().borrow_mut().deref_mut() {
                YoloValueType::Constant(_) => Err(InterpreterError::CannotAssingnToConstant {
                    name: name.to_string(),
                }),
                YoloValueType::Mutable(value) => {
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
        debug!("Creating new scope");
        self.parent = Some(Box::new(self.clone()));
        self.values = HashMap::new();
        debug!("Values: {:#?}", self._get_all_keys());
        debug!("Number of parents after: {}", self._get_number_of_parents());
    }
    pub fn restore_scope(&mut self) {
        if self.parent.is_none() {
            return;
        }
        debug!("Restoring scope");
        debug!(
            "Number of parents before: {}",
            self._get_number_of_parents()
        );
        self.values = self.parent.as_mut().unwrap().values.clone();
        self.parent = self.parent.take().unwrap().parent.take();
        debug!("Number of parents after: {}", self._get_number_of_parents());
        debug!("Values: {:#?}", self._get_all_keys());
    }

    pub fn _get_all_keys(&self) -> Vec<String> {
        let mut keys = self.values.keys().cloned().collect::<Vec<String>>();
        if let Some(parent) = &self.parent {
            keys.extend(parent._get_all_keys());
        }
        keys
    }

    pub fn _get_number_of_parents(&self) -> u32 {
        if self.parent.is_none() {
            0
        } else {
            1 + self.parent.as_ref().unwrap()._get_number_of_parents()
        }
    }
}

/// The type of a Yolo value.
/// Either a constant value or a mutable value.
#[derive(Debug, Clone)]
pub enum YoloValueType {
    Constant(YoloValue),
    Mutable(YoloValue),
}

/// The internal representation of a Yolo value.
#[derive(Debug, Clone)]
pub enum YoloValue {
    Number(OrderedFloat<f64>),
    String(String),
    Boolean(bool),
    ClojureFunction {
        name: String,
        body: Option<Box<parser::Stmt>>,
        captured_variables: HashMap<String, Rc<RefCell<YoloValueType>>>,
        parameters: Vec<String>,
    },
    BuiltinFunction {
        name: String,
        parameters: Vec<String>,
        function: fn(&mut Interpreter, &[YoloValue]) -> Result<YoloValue, InterpreterError>,
    },
    Nil,
}

impl PartialEq for YoloValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (YoloValue::Number(left), YoloValue::Number(right)) => left == right,
            (YoloValue::String(left), YoloValue::String(right)) => left == right,
            (YoloValue::Boolean(left), YoloValue::Boolean(right)) => left == right,
            (YoloValue::Nil, YoloValue::Nil) => true,
            _ => false,
        }
    }
}

impl YoloValue {
    fn is_truthy(&self) -> bool {
        match self {
            YoloValue::Nil => false,
            YoloValue::Boolean(value) => *value,
            YoloValue::Number(value) => !value.is_nan() && !value.is_infinite() && *value != 0.0,
            _ => true,
        }
    }

    fn name(&self) -> String {
        match self {
            YoloValue::Number(_) => "number".to_string(),
            YoloValue::String(_) => "string".to_string(),
            YoloValue::Boolean(_) => "boolean".to_string(),
            YoloValue::ClojureFunction { .. } => "function".to_string(),
            YoloValue::BuiltinFunction { .. } => "function".to_string(),
            YoloValue::Nil => "nil".to_string(),
        }
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: &[YoloValue],
    ) -> Result<YoloValue, InterpreterError> {
        return match self {
            YoloValue::ClojureFunction {
                body,
                parameters,
                captured_variables,
                ..
            } => {
                debug!(
                    "Calling function with parameters: {:#?} and arguments: {:#?}",
                    parameters, arguments
                );

                if arguments.len() != parameters.len() {
                    return Err(InterpreterError::InvalidArgumentCount {
                        expected: parameters.len(),
                        found: arguments.len(),
                    });
                }

                debug!("captured variables: {:#?}", captured_variables);
                interpreter.environment.new_scope();
                // Add the captured variables to the environment
                for (name, value) in captured_variables.iter() {
                    interpreter
                        .environment
                        .inject_variable(name.to_owned(), value);
                }

                // Add the arguments to the environment
                for (parameter, argument) in parameters.iter().zip(arguments.iter()) {
                    interpreter
                        .environment
                        .define_constant(parameter.to_owned(), argument.clone());
                }

                let interpreter_result = interpreter.interpret_statement(&body.clone().unwrap());

                interpreter.environment.restore_scope();
                let return_value: Result<YoloValue, InterpreterError> = match interpreter_result {
                    Ok(_value) => Ok(YoloValue::Nil),
                    Err(InterpreterError::Return { value }) => Ok(value),
                    Err(error) => Err(error),
                };
                debug!("Return value: {:#?}", return_value);

                return_value
            }
            YoloValue::BuiltinFunction {
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

impl std::fmt::Display for YoloValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            YoloValue::Number(value) => write!(f, "{}", value),
            YoloValue::String(value) => write!(f, "{}", value),
            YoloValue::Boolean(value) => write!(f, "{}", value),
            YoloValue::ClojureFunction { name, .. } => write!(f, "<fn {}>", name),
            YoloValue::BuiltinFunction { name, .. } => write!(f, "<fn {}>", name),
            YoloValue::Nil => write!(f, "nil"),
        }
    }
}
