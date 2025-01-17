
//
// Lox
//

pub struct Lox {
    had_error: bool,
}

impl Lox {
    pub fn new() -> Lox {
        Lox { had_error: false }
    }
    pub fn run_file(&mut self, filepath: &str) -> Result<(), LoxError> {

        let source_result = std::fs::read_to_string(filepath);

        if source_result.is_err() {
            return Err(LoxError::FileError(format!("Could not read file: {}", filepath)));
        }
        let source = source_result.unwrap();

        self.run(&source)?;
        Ok(())
    }

    pub fn run_prompt(&mut self) -> Result<(), LoxError> {

        loop {
            let mut line = String::new();
            let read_result = std::io::stdin().read_line(&mut line);

            if read_result.is_err() {
                return Err(LoxError::InternalError(format!("Could not read from stdin")));
            }

            // Check for EOF
            if line.trim().is_empty() {
                break;
            }

            self.run(&line)?;
        }
        Ok(())
    }

    fn run(&mut self, source: &str) -> Result<(),LoxError> {
        // Get tokens
        let mut scanner = Scanner::new(source);
        let (tokens, had_error) = scanner.scan_tokens();
        self.had_error = had_error;
        
        
        println!("{:?}", tokens);
        println!("{}", self.had_error);
        return Ok(());
    }
}

//
// Scanner
//

pub struct Scanner {
    source: String,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: &str) -> Scanner {
        Scanner {
            source: source.to_string(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> (Vec<Token>, bool) {
        let mut tokens: Vec<Token> = Vec::new();
        let mut had_error = false;

        while !self.is_at_end() {
            self.start = self.current;
            match self.scan_token() {
                Ok(Some(token)) => tokens.push(token),
                Ok(None) => {},
                Err(error) => {
                    // TODO: Handle errors
                    let err = LoxError::UnexpectedChar(error.message(), self.line);
                    println!("{}", err);
                    had_error = true;
                }
            }
        }

        return (tokens,had_error);
    }

    fn scan_token(&mut self) -> Result<Option<Token>, ScannerError> {
        let c = self.advance();
        match c {
            '(' => Ok(Some(Token {
                lexeme: c.to_string(),
                token_type: TokenType::LeftParen,
                line: self.line
            })),
            ')' => Ok(Some(Token {
                lexeme: c.to_string(),
                token_type: TokenType::RightParen,
                line: self.line
            })),
            '{' => Ok(Some(Token {
                lexeme: c.to_string(),
                token_type: TokenType::LeftBrace,
                line: self.line
            })),
            '}' => Ok(Some(Token {
                lexeme: c.to_string(),
                token_type: TokenType::RightBrace,
                line: self.line
            })),
            ',' => Ok(Some(Token {
                lexeme: c.to_string(),
                token_type: TokenType::Comma,
                line: self.line
            })),
            '.' => Ok(Some(Token {
                lexeme: c.to_string(),
                token_type: TokenType::Dot,
                line: self.line
            })),
            '-' => Ok(Some(Token {
                lexeme: c.to_string(),
                token_type: TokenType::Minus,
                line: self.line
            })),
            '+' => Ok(Some(Token {
                lexeme: c.to_string(),
                token_type: TokenType::Plus,
                line: self.line
            })),
            ';' => Ok(Some(Token {
                lexeme: c.to_string(),
                token_type: TokenType::Semicolon,
                line: self.line
            })),
            '*' => Ok(Some(Token {
                lexeme: c.to_string(),
                token_type: TokenType::Star,
                line: self.line
            })),
            '!' => {
                let token_type = if self.match_char('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };
                let lexeme = c.to_string() + &self.source.chars().nth(self.current -1).unwrap().to_string();
                Ok(Some(Token {
                    lexeme,
                    token_type,
                    line: self.line
                }))
            },
            '=' => {
                let token_type = if self.match_char('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };
                let lexeme = c.to_string() + &self.source.chars().nth(self.current -1).unwrap().to_string();
                Ok(Some(Token {
                    lexeme,
                    token_type,
                    line: self.line
                }))
            },
            '<' => {
                let token_type = if self.match_char('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                let lexeme = c.to_string() + &self.source.chars().nth(self.current - 1).unwrap().to_string();
                Ok(Some(Token {
                    lexeme,
                    token_type,
                    line: self.line
                }))
            },
            '>' => {
                let token_type = if self.match_char('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                let lexeme = c.to_string() + &self.source.chars().nth(self.current -1).unwrap().to_string();
                Ok(Some(Token {
                    lexeme,
                    token_type,
                    line: self.line
                }))
            },
            '/' => {
                if self.match_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                    Ok(None)
                } else {
                    Ok(Some(Token {
                        lexeme: c.to_string(),
                        token_type: TokenType::Slash,
                        line: self.line
                    }))
                }
            },
            ' ' | '\r' | '\t' => Ok(None),
            '\n' => {
                self.line += 1;
                Ok(None)
            },
            _ => {
                return Err(ScannerError::UnexpectedCharacter(c));
            }
        }
    }

    fn match_char(&mut self, c: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source.chars().nth(self.current).unwrap() != c {
            return false;
        }
        self.current += 1;
        return true;
    }

    fn peek(&mut self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        return self.source.chars().nth(self.current).unwrap();
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        return self.source.chars().nth(self.current - 1).unwrap_or(' ');
    }

    fn is_at_end(&self) -> bool {
        return self.current >= self.source.len();
    }

}

#[derive(Debug)]
pub struct Token {
    pub lexeme: String,
    pub token_type: TokenType,
    pub line: usize
}

#[derive(Debug)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier,
    String,
    Number,
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    EOF
}

//
// Errors
//

#[derive(Debug)]
pub enum LoxError {
    FileError(String),
    InternalError(String),
    UnexpectedChar(String,usize)
}

impl LoxError {
    pub fn message(&self) -> String {
        match self {
            LoxError::FileError(msg) => msg.to_string(),
            LoxError::InternalError(msg) => msg.to_string(),
            LoxError::UnexpectedChar(character, line) => format!("Unexpected character: {} at line {}", character, line)
        }
    }
}

impl std::fmt::Display for LoxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Lox Error: {}", self.message())
    }
    
}
impl std::error::Error for LoxError {}


enum ScannerError {
    UnexpectedCharacter(char)
}
impl ScannerError {
    pub fn message(&self) -> String {
        match self {
            ScannerError::UnexpectedCharacter(c) => c.to_string()
        }
    }
}

impl std::fmt::Display for ScannerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Scanner Error: Unexpected character: {}", self)
    }
    
}