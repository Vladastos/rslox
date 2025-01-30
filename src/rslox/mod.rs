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
        let source_result = std::fs::read_to_string(filepath);

        if source_result.is_err() {
            return Err(LoxError::FileError(filepath.to_string()));
        }
        let source = source_result.unwrap();

        self.run(&source)?;
        Ok(())
    }

    pub fn run_prompt(&mut self) -> Result<(), LoxError> {
        loop {
            let mut line = String::new();
            print!("> ");
            std::io::stdout().flush().unwrap();
            let read_result = std::io::stdin().read_line(&mut line);

            if read_result.is_err() {
                return Err(LoxError::IoError(read_result.unwrap_err()));
            }

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
    #[error("Could not open file {_0}")]
    FileError(String),
    #[error("Syntax error")]
    ScanningError(
        #[source]
        #[from]
        ScannerError,
    ),
    // this is an horrible way to solve the problem of multiple sources
    #[error("Syntax error: {}", _0.into_iter().map(|e| format!("\t{} \n", e)).collect::<String>())]
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
    #[error("Unexpected token: {0} expected: {1} [line {2} column {3}]")]
    UnexpectedToken(TokenType, TokenType, usize, usize),
    #[error("Unexpected token: {0} [line {1} column {2}]")]
    UnexpectedTokenNoExpected(TokenType, usize, usize),
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
    #[error("{0}")]
    RuntimeError(String),
}
