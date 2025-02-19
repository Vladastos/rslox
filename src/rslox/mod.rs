mod builtins;
mod interpreter;
mod parser;
mod scanner;

#[cfg(test)]
mod tests;

use std::{
    io::Write,
    path::{Path, PathBuf},
};

use interpreter::Environment;
use log::error;
use scanner::TokenType;
use thiserror::Error;

/// Lox

pub struct Lox;

impl Lox {
    pub fn new() -> Lox {
        Lox
    }
    pub fn run_file(&mut self, path: &Path) -> Result<(), LoxError> {
        let source = std::fs::read_to_string(path).map_err(|source| LoxError::FileError {
            path: path.to_owned(),
            source,
        })?;
        let mut environment = Environment::new();

        self.run(&source, &mut environment)?;
        Ok(())
    }

    pub fn run_prompt(&mut self) -> Result<(), LoxError> {
        let mut environment = Environment::new();
        loop {
            let mut line = String::new();
            print!("> ");
            std::io::stdout().flush().unwrap();
            std::io::stdin().read_line(&mut line)?;

            // Check for Eof
            if line.trim().is_empty() {
                break;
            }

            let result = self.run(&line, &mut environment);

            if result.is_err() {
                println!("{}", result.unwrap_err());
            }
        }
        Ok(())
    }

    fn run(&mut self, source: &str, environment: &mut Environment) -> Result<(), LoxError> {
        // Scan the source code into tokens
        let tokens = scanner::Scanner::new(source).scan_tokens()?;

        // Parse the tokens
        let parse_result = parser::Parser::new(tokens).parse()?;

        // Run the statements
        interpreter::Interpreter::new(environment).run(&parse_result)?;

        Ok(())
    }

    /// Parses the source code, and returns the result as an `Ok(())` if successful.
    ///
    /// Used for testing code without running it.
    #[allow(dead_code)]
    pub fn parse(&mut self, source: &str) -> Result<(), LoxError> {
        parser::Parser::new(scanner::Scanner::new(source).scan_tokens()?)
            .parse()
            .map_err(|errors| LoxError::ParsingError(errors))
            .map(|_| ())
    }
}

/// Errors

#[derive(Debug, Error)]
pub enum LoxError {
    #[error("Could not open file {}", path.display())]
    FileError {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },
    #[error("Syntax error: {}", _0)]
    ScannerError(
        #[source]
        #[from]
        ScannerError,
    ),
    // this is an horrible way to solve the problem of multiple sources
    #[error("Syntax error: {}", _0.iter().map(|e| format!("\t{e} \n")).collect::<String>())]
    ParsingError(Vec<ParserError>),
    #[error("Runtime error: {0}")]
    RuntimeError(
        #[source]
        #[from]
        InterpreterError,
    ),
    #[error("IO error")]
    IoError(
        #[source]
        #[from]
        std::io::Error,
    ),
}

impl From<Vec<ParserError>> for LoxError {
    fn from(errors: Vec<ParserError>) -> Self {
        LoxError::ParsingError(errors)
    }
}

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Unexpected token: {found} expected: {expected} [line {line} column {column}]")]
    UnexpectedToken {
        found: TokenType,
        expected: TokenType,
        line: usize,
        column: usize,
    },
    #[error("Unexpected token: {token_type} [line {line} column {column}]")]
    UnexpectedTokenNoExpected {
        token_type: TokenType,
        line: usize,
        column: usize,
    },
    #[error("Invalid assignment target ")]
    InvalidAssignmentTarget { line: usize, column: usize },

    #[error("Unterminated block [line {line} column {column}]")]
    UnterminatedBlock { line: usize, column: usize },

    #[error("Too many arguments [line {line} column {column}]")]
    TooManyArguments { line: usize, column: usize },

    #[error("Unitialized variable {name} [line {line} column {column}]")]
    UninitializedVariable {
        name: String,
        line: usize,
        column: usize,
    },
}

#[derive(Error, Debug)]
pub enum ScannerError {
    #[error("Unexpected character: {0} at line {1}")]
    UnexpectedCharacter(char, usize),
    #[error("Unterminated string at line {0}")]
    UnterminatedString(usize),
    #[error("Unterminated comment at line {0}")]
    UnterminatedComment(usize),
}

#[derive(Error, Debug)]
pub enum InterpreterError {
    #[error("Invalid operand type: {found} expected: {expected}")]
    InvalidOperandType {
        found: String,
        expected: &'static str,
    },
    #[error("Undefined variable: {name}")]
    UndefinedVariable { name: String },
    #[error("Division by zero")]
    DivisionByZero,

    #[error("Cannot call non-function: {name}")]
    NonFunctionCall { name: String },
    #[error("Cannot assign to constant: {name}")]
    CannotAssingnToConstant { name: String },

    #[error("Invalid argument count: {found} expected: {expected}")]
    InvalidArgumentCount { found: usize, expected: usize },

    /// This error is returned when a function returns a value
    #[error("Return value: {value}")]
    Return { value: interpreter::LoxValue },
}
