mod scanner;
use std::io::Write;

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
            return Err(LoxError::FileError(format!(
                "Could not read file: {}",
                filepath
            )));
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

#[derive(Debug)]
pub enum LoxError {
    FileError(String),
    InternalError(String),
}

impl LoxError {
    pub fn message(&self) -> String {
        match self {
            LoxError::FileError(msg) => msg.to_string(),
            LoxError::InternalError(msg) => msg.to_string(),
        }
    }
}

impl std::fmt::Display for LoxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error: {}", self.message())
    }
}
impl std::error::Error for LoxError {}
