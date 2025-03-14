use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{
    interpreter::{Environment, LoxValueType},
    parser, InterpreterError,
};

/// Captures variables in a function body and stores them in a HashMap.
/// It does this by recursively evaluating the body of the function in search of variable usages.
/// The HashMap is then returned.
pub struct Capturer<'a> {
    environment: &'a Environment,
    pub captured_variables: HashMap<String, Rc<RefCell<LoxValueType>>>,
}

impl Capturer<'_> {
    pub fn new(environment: &Environment) -> Capturer {
        Capturer {
            environment,
            captured_variables: HashMap::new(),
        }
    }
    /// This function recursively evaluates the body of a function in search of variable usages, pretty much ignoring everything else.
    pub fn capture(
        &mut self,
        body: &parser::Stmt,
    ) -> Result<HashMap<String, Rc<RefCell<LoxValueType>>>, InterpreterError> {
        self.capture_statement(body)?;
        Ok(self.captured_variables.clone())
    }
    fn capture_statement(&mut self, statement: &parser::Stmt) -> Result<(), InterpreterError> {
        match statement {
            parser::Stmt::MutDeclaration { initializer, .. } => {
                if let Some(initializer) = initializer {
                    self.capture_expression(initializer)?;
                }

                Ok(())
            }
            parser::Stmt::Expression { expression } => {
                self.capture_expression(expression)?;
                Ok(())
            }
            parser::Stmt::Print { expression } => {
                self.capture_expression(expression)?;
                Ok(())
            }
            parser::Stmt::ConstDeclaration { initializer, .. } => {
                self.capture_expression(initializer)?;
                Ok(())
            }
            parser::Stmt::Block { statements } => {
                for statement in statements {
                    self.capture_statement(statement)?;
                }
                Ok(())
            }
            parser::Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.capture_expression(condition)?;
                self.capture_statement(then_branch)?;
                if let Some(else_branch) = else_branch {
                    self.capture_statement(else_branch)?;
                }
                Ok(())
            }
            parser::Stmt::While { condition, body } => {
                self.capture_expression(condition)?;
                self.capture_statement(body)?;
                Ok(())
            }
            parser::Stmt::Function { body, .. } => {
                self.capture_statement(body)?;
                Ok(())
            }
            parser::Stmt::Return { value } => {
                if let Some(value) = value {
                    self.capture_expression(value)?;
                }
                Ok(())
            }
        }
    }

    fn capture_expression(&mut self, expression: &parser::Expr) -> Result<(), InterpreterError> {
        match expression {
            parser::Expr::Variable { name } => {
                // If the variable is not found in the environment passed to the capturer, it probably means that the variable is declared inside the function (or does not exist at all) so we can ignore it
                if let Some(value) = self.environment.extract_variable(name) {
                    // This is the only thing that we care about in the capturer
                    // We could also return an error here if the variable is not a constant,
                    // but it would be better to do it in a previous step and not at runtime
                    self.captured_variables.insert(name.to_owned(), value);
                }
                Ok(())
            }
            parser::Expr::Assignment { value, .. } => {
                self.capture_expression(value)?;
                Ok(())
            }
            parser::Expr::Binary { left, right, .. } => {
                self.capture_expression(left)?;
                self.capture_expression(right)?;
                Ok(())
            }
            parser::Expr::Unary { right, .. } => {
                self.capture_expression(right)?;
                Ok(())
            }
            parser::Expr::Literal { .. } => Ok(()),
            parser::Expr::Grouping { expression } => self.capture_expression(expression),
            parser::Expr::Call {
                callee, arguments, ..
            } => {
                self.capture_expression(callee)?;
                for argument in arguments {
                    self.capture_expression(argument)?;
                }
                Ok(())
            }
        }
    }
}
