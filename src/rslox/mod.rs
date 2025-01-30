mod parser;
mod scanner;
use std::io::Write;

use log::{debug, error, info};
use scanner::TokenType;
use thiserror::Error;

//
// Lox
//

pub struct Lox {}

impl Lox {
    pub fn new() -> Lox {
        Lox {}
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
                return Err(LoxError::RuntimeError(format!(
                    "Could not read from stdin"
                )));
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
        debug!("Parsed expression: {:#?}", parse_result);

        return Ok(());
    }
}

//
// Errors
//

#[derive(Debug,)]
pub enum LoxError {
    FileError(String),
    ScanningError(ScannerError),
    ParsingError(Vec<ParserError>),
    RuntimeError(String),
}
impl std::fmt::Display for LoxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxError::FileError(path) => write!(f, "Error: Could not open file: {}", path),
            LoxError::ScanningError(errors) => {
                write!(f, "Syntax error: Encountered the following error during scanning: {:#?}", errors)
            }
            LoxError::ParsingError(errors) => {
                write!(f, "Syntax error: {:#?}", errors)
            }
            LoxError::RuntimeError(message) => write!(f, "Runtime error: {}", message),
            
        }
    }
    
}

impl From<Vec<ParserError>> for LoxError {
    fn from(errors: Vec<ParserError>) -> Self {
        LoxError::ParsingError(errors)
    }
    
}

impl From<ScannerError> for LoxError {
    fn from(error: ScannerError) -> Self {
        LoxError::ScanningError(error)
    }
    
}


#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Unexpected token: {0} expected: {1} [line {2}]")]
    UnexpectedToken(TokenType, TokenType, usize),
    #[error("Unexpected token: {0} [line {1}]")]
    UnexpectedTokenNoExpected(TokenType, usize),
}

#[derive(Error, Debug)]
pub enum ScannerError {
    #[error("Unexpected character: {0} at line {1}")]
    UnexpectedCharacter(char, usize),
    #[error("Unterminated string: {0} at line {1}")]
    UnterminatedString(String, usize),
    #[error("Unterminated comment at line {0}")]
    UnterminatedComment(usize),

}
