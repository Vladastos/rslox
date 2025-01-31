#[cfg(test)]
use crate::rslox::*;

#[test]
fn hello_world() {
    let mut lox = Lox::new();
    lox.run_file(std::path::Path::new("tests/hello-world.lox"))
        .unwrap();
}

#[test]
fn comments() {
    let mut lox = Lox::new();
    lox.run_file(std::path::Path::new("tests/comments.lox"))
        .unwrap();
}

#[test]
fn unterminated_string() {
    let mut lox = Lox::new();
    let result = lox.run_file(std::path::Path::new("tests/unterminated-string.lox"));

    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().to_string(),
        "Syntax error: Unterminated string at line 2"
    );
}

#[test]
fn non_existent_file() {
    let mut lox = Lox::new();
    let result = lox.run_file(std::path::Path::new("tests/non-existent.lox"));
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err().to_string(),
        "Could not open file tests/non-existent.lox"
    );
}
