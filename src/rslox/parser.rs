use log::trace;

use crate::rslox::scanner;

//
// Parser
//

pub struct Parser {}

impl Parser {
    pub fn new() -> Parser {
        Parser {}
    }

    pub fn parse(&mut self, tokens: Vec<scanner::Token>) -> Result<Expression, ()> {
        trace!("Parsing tokens: {:#?}", tokens);

        let expression = Expression::new();
        Ok(expression)
    }
}


//
// Expression
//

pub struct Expression {}

impl Expression {
    pub fn new() -> Expression {
        Expression {}
    }
}