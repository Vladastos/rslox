mod interpreter;
mod parser;
mod scanner;

use std::io::Write;

use log::{debug, error};
use scanner::TokenType;
use thiserror::Error;

/// Lox

pub struct Lox;

impl Lox {
    pub fn new() -> Lox {
        Lox
    }
    pub fn run_file(&mut self, filepath: &str) -> Result<(), LoxError> {
        let source = std::fs::read_to_string(filepath).map_err(|source| LoxError::FileError {
            path: filepath.to_owned(),
            source,
        })?;

        self.run(&source)?;
        Ok(())
    }

    pub fn run_prompt(&mut self) -> Result<(), LoxError> {
        loop {
            let mut line = String::new();
            print!("> ");
            std::io::stdout().flush().unwrap();
            std::io::stdin().read_line(&mut line)?;

            // Check for EOF
            if line.trim().is_empty() {
                break;
            }

            let result = self.run(&line);

            if result.is_err() {
                println!("{}", result.unwrap_err());
            }
        }
        Ok(())
    }

    fn run(&mut self, source: &str) -> Result<(), LoxError> {
        // Scan the source code into tokens
        debug!("Scanning source code");
        let tokens = scanner::Scanner::new(source).scan_tokens()?;

        // Parse the tokens
        let parse_result = parser::Parser::new(tokens).parse()?;
        debug!("Parsed statements: {:#?}", parse_result);

        // Run the statements
        interpreter::Interpreter::new().run(&parse_result)?;

        Ok(())
    }
}

/// Errors

#[derive(Debug, Error)]
pub enum LoxError {
    #[error("Could not open file {path}")]
    FileError {
        path: String,
        #[source]
        source: std::io::Error,
    },
    #[error("Syntax error")]
    ScanningError(
        #[source]
        #[from]
        ScannerError,
    ),
    // this is an horrible way to solve the problem of multiple sources
    #[error("Syntax error: {}", _0.iter().map(|e| format!("\t{} \n", e)).collect::<String>())]
    ParsingError(Vec<ParserError>),
    #[error("Runtime error")]
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
}

#[derive(Error, Debug)]
pub enum ScannerError {
    #[error("Unexpected character: {0} at line {1}")]
    UnexpectedCharacter(char, usize),
    #[error("Unterminated string: at line {0}")]
    UnterminatedString(usize),
    #[error("Unterminated comment at line {0}")]
    UnterminatedComment(usize),
}

#[derive(Error, Debug)]
pub enum InterpreterError {
    #[error("Operands must be numbers")]
    OperandsMustBeNumbers,
}
