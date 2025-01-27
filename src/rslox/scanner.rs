use std::collections::hash_map::HashMap;
use std::sync::LazyLock;

use thiserror::Error;

static KEYWORDS: LazyLock<HashMap<&'static str, TokenType>> = LazyLock::new(|| {
    HashMap::from([
        ("and", TokenType::And),
        ("class", TokenType::Class),
        ("else", TokenType::Else),
        ("false", TokenType::False),
        ("for", TokenType::For),
        ("fun", TokenType::Fun),
        ("if", TokenType::If),
        ("nil", TokenType::Nil),
        ("or", TokenType::Or),
        ("print", TokenType::Print),
        ("return", TokenType::Return),
        ("super", TokenType::Super),
        ("this", TokenType::This),
        ("true", TokenType::True),
        ("var", TokenType::Var),
        ("while", TokenType::While),
    ])
});

//
// Scanner
//

pub struct Scanner {
    source: String,
    source_length: usize,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: &str) -> Scanner {
        Scanner {
            source: source.to_string(),
            source_length: source.chars().count(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, Vec<Token>> {
        let mut tokens: Vec<Token> = Vec::new();
        let mut had_error = false;

        while !self.is_at_end() {
            self.start = self.current;
            match self.scan_token() {
                Ok(Some(token)) => tokens.push(token),
                Ok(None) => {}
                Err(error) => {
                    println!("{}", error);
                    had_error = true;
                }
            }
        }

        tokens.push(Token {
            lexeme: "".to_string(),
            token_type: TokenType::EOF,
            literal: None,
            line: self.line,
        });

        return if had_error { Err(tokens) } else { Ok(tokens) };
    }

    fn scan_token(&mut self) -> Result<Option<Token>, ScannerError> {
        let c = self.advance();
        match c {
            '(' => Ok(Some(Token {
                lexeme: c.to_string(),
                token_type: TokenType::LeftParen,
                literal: None,
                line: self.line,
            })),
            ')' => Ok(Some(Token {
                lexeme: c.to_string(),
                token_type: TokenType::RightParen,
                literal: None,
                line: self.line,
            })),
            '{' => Ok(Some(Token {
                lexeme: c.to_string(),
                token_type: TokenType::LeftBrace,
                literal: None,
                line: self.line,
            })),
            '}' => Ok(Some(Token {
                lexeme: c.to_string(),
                token_type: TokenType::RightBrace,
                literal: None,
                line: self.line,
            })),
            ',' => Ok(Some(Token {
                lexeme: c.to_string(),
                token_type: TokenType::Comma,
                literal: None,
                line: self.line,
            })),
            '.' => Ok(Some(Token {
                lexeme: c.to_string(),
                token_type: TokenType::Dot,
                literal: None,
                line: self.line,
            })),
            '-' => Ok(Some(Token {
                lexeme: c.to_string(),
                token_type: TokenType::Minus,
                literal: None,
                line: self.line,
            })),
            '+' => Ok(Some(Token {
                lexeme: c.to_string(),
                token_type: TokenType::Plus,
                literal: None,
                line: self.line,
            })),
            ';' => Ok(Some(Token {
                lexeme: c.to_string(),
                token_type: TokenType::Semicolon,
                literal: None,
                line: self.line,
            })),
            '*' => Ok(Some(Token {
                lexeme: c.to_string(),
                token_type: TokenType::Star,
                literal: None,
                line: self.line,
            })),
            '!' => {
                let token_type = if self.match_char('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };
                let lexeme = if token_type == TokenType::Bang {
                    "!".to_string()
                } else {
                    "!=".to_string()
                };
                Ok(Some(Token {
                    lexeme,
                    token_type,
                    literal: None,
                    line: self.line,
                }))
            }
            '=' => {
                let token_type = if self.match_char('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };
                let lexeme = if token_type == TokenType::Equal {
                    "=".to_string()
                } else {
                    "==".to_string()
                };
                Ok(Some(Token {
                    lexeme,
                    token_type,
                    literal: None,
                    line: self.line,
                }))
            }
            '<' => {
                let token_type = if self.match_char('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                let lexeme = if token_type == TokenType::Less {
                    "<".to_string()
                } else {
                    "<=".to_string()
                };
                Ok(Some(Token {
                    lexeme,
                    token_type,
                    literal: None,
                    line: self.line,
                }))
            }
            '>' => {
                let token_type = if self.match_char('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                let lexeme = if token_type == TokenType::Greater {
                    ">".to_string()
                } else {
                    ">=".to_string()
                };
                Ok(Some(Token {
                    lexeme,
                    token_type,
                    literal: None,
                    line: self.line,
                }))
            }
            '/' => {
                if self.match_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                    Ok(None)
                } else if self.match_char('*') {
                    self.scan_multi_line_comment()?;
                    Ok(None)
                } else {
                    Ok(Some(Token {
                        lexeme: c.to_string(),
                        token_type: TokenType::Slash,
                        literal: None,
                        line: self.line,
                    }))
                }
            }
            ' ' | '\r' | '\t' => Ok(None),
            '\n' => {
                self.line += 1;
                Ok(None)
            }
            '"' => {
                let lexeme = self.scan_string()?;
                Ok(Some(Token {
                    lexeme: lexeme.to_string(),
                    token_type: TokenType::String,
                    literal: Option::from(lexeme),
                    line: self.line,
                }))
            }
            '0'..='9' => {
                let lexeme = self.scan_number()?;
                Ok(Some(Token {
                    lexeme: lexeme.to_string(),
                    token_type: TokenType::Number,
                    literal: Option::from(lexeme),
                    line: self.line,
                }))
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let lexeme = self.scan_identifier(c)?;
                let token_type = match (*KEYWORDS).get(&lexeme.as_str()) {
                    Some(t) => *t,
                    None => TokenType::Identifier,
                };
                Ok(Some(Token {
                    lexeme: lexeme.to_string(),
                    token_type,
                    literal: Option::from(lexeme),
                    line: self.line,
                }))
            }
            _ => {
                return Err(ScannerError::UnexpectedCharacter(c, self.line));
            }
        }
    }

    fn scan_multi_line_comment(&mut self) -> Result<Option<Token>, ScannerError> {
        while self.peek() != '*' && self.peek_next() != '/' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }
        if self.is_at_end() {
            return Err(ScannerError::UnterminatedComment(self.line));
        }
        self.advance();
        self.advance();
        Ok(None)
    }

    fn scan_identifier(&mut self, c: char) -> Result<String, ScannerError> {
        let mut result = String::from(c);
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            result.push(self.advance());
        }
        Ok(result)
    }

    fn scan_string(&mut self) -> Result<String, ScannerError> {
        let mut result = String::new();
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            result.push(self.advance());
        }
        if self.is_at_end() {
            return Err(ScannerError::UnterminatedString(result, self.line));
        }
        self.advance();
        return Ok(result);
    }

    fn scan_number(&mut self) -> Result<String, ScannerError> {
        let mut result = String::from(self.source.chars().nth(self.start).unwrap().to_string());
        while self.peek().is_ascii_digit() {
            result.push(self.advance());
        }
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            result.push(self.advance());
            while self.peek().is_ascii_digit() {
                result.push(self.advance());
            }
        }
        return Ok(result);
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

    fn peek_next(&mut self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        if self.current + 1 >= self.source_length {
            return '\0';
        }
        return self.source.chars().nth(self.current + 1).unwrap();
    }

    fn advance(&mut self) -> char {
        let c = self.source.chars().nth(self.current).unwrap();
        self.current += 1;
        return c;
    }

    fn is_at_end(&self) -> bool {
        let is_at_end = self.current >= self.source_length;
        return is_at_end;
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub lexeme: String,
    pub literal: Option<String>,
    pub token_type: TokenType,
    pub line: usize,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.lexeme)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
    EOF,
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
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
