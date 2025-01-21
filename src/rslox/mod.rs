mod scanner;
use std::io::Write;

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
                return Err(LoxError::InternalError(format!(
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
        // Get tokens
        let mut scanner = scanner::Scanner::new(source);

        let scan_result = scanner.scan_tokens();

        if scan_result.is_err() {
            return Err(LoxError::InternalError(format!("Invalid syntax")));
        }

        let tokens = scan_result.unwrap();

        println!("{:?}", tokens);
        return Ok(());
    }
}

//
// Errors
//

#[derive(Error, Debug)]
pub enum LoxError {
    #[error("Could not read file: {}", .0)]
    FileError(String),
    #[error("Error: {}", .0)]
    InternalError(String),
}
