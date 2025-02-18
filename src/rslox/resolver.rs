use super::parser;
use super::Environment;
use super::ResolverError;

/// Resolver
/// Prepares the environment for running the program and ensures that all identifiers are defined correctly
pub struct Resolver<'env> {
    environment: &'env mut Environment,
}

impl Resolver<'_> {
    pub fn new(env: &mut Environment) -> Resolver {
        Resolver { environment: env }
    }

    pub fn resolve(&mut self, statements: &[parser::Stmt]) -> Result<(), ResolverError> {
        Ok(())
    }
}
