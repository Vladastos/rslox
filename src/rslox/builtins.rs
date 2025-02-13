//! TODO:
//!  - Add more built-in functions
//!
use std::{collections::HashMap, sync::LazyLock};

use ordered_float::OrderedFloat;

use super::interpreter::LoxValue;

/// A lazy-initialized map of built-in functions.
/// The map is initialized on first use.
/// Used to define built-in functions in the interpreter.
pub static BUILTINS: LazyLock<HashMap<String, LoxValue>> = LazyLock::new(|| {
    HashMap::from([(
        "clock".to_string(),
        LoxValue::BuiltinFunction {
            name: "clock".to_string(),
            parameters: vec![],
            function: |_, _| {
                Ok(LoxValue::Number(OrderedFloat::from(
                    std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .unwrap()
                        .as_millis() as f64,
                )))
            },
        },
    )])
});
