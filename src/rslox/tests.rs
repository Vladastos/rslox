use std::{ffi::OsStr, os::unix::ffi::OsStrExt};

use crate::rslox::*;

// Include generated test code
include!(concat!(env!("OUT_DIR"), "/tests.rs"));

fn test_impl(
    Test {
        name,
        outcome: expected,
        source,
        parse_only,
    }: Test,
) {
    let test_file_path = std::path::Path::new(OsStr::from_bytes(&source));

    let mut lox = Lox::new();
    let result = match parse_only {
        true => lox.parse(&std::fs::read_to_string(test_file_path).unwrap()),
        false => lox.run_file(test_file_path),
    };

    match (expected, result) {
        (TestOutcome::Success, Ok(())) => (),
        (TestOutcome::Success, Err(err)) => {
            panic!(
                "Test {name} from file {} returned an error, instead of succeding: {err}",
                test_file_path.display()
            )
        }
        (TestOutcome::Failure { .. }, Ok(())) => {
            panic!(
                "Test {name} from file {} succeded, an error was expected.",
                test_file_path.display()
            )
        }
        (
            TestOutcome::Failure {
                message: Some(message),
            },
            Err(err),
        ) => {
            let err = err.to_string();
            if message != err {
                panic!(
                        "Test {name} from file {} got the wrong error: expected `{message}`, got `{err}`",
                        test_file_path.display()
                    )
            }
        }
        (TestOutcome::Failure { message: None }, Err(_)) => (),
    }
}
