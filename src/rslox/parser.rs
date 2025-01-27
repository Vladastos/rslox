use log::trace;

use crate::rslox::scanner;

//
// Parser
//

pub struct Parser {
    tokens: Vec<scanner::Token>,
    current: usize,
    had_error: bool,
}

impl Parser {
    pub fn new(tokens: Vec<scanner::Token>) -> Parser {
        Parser {
            tokens,
            current: 0,
            had_error: false,
        }
    }

    pub fn parse(&mut self) -> Result<Expr, ()> {
        trace!("Parsing tokens: {:#?}", self.tokens);
        let result = Expr::Literal {
            value: "hello".to_string(),
        };

        Ok(result)
    }
}

//
// Expression
//

#[derive(Debug, Clone)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: scanner::Token,
        right: Box<Expr>,
    },
    Grouping {
        expression: Box<Expr>,
    },
    Literal {
        value: String,
    },
    Unary {
        operator: scanner::Token,
        right: Box<Expr>,
    },
}

// TODO: Finish the parser