use std::{collections::HashMap, sync::LazyLock};

use ordered_float::OrderedFloat;

use super::interpreter::LoxValue;

pub static BUILTINS: LazyLock<HashMap<String, LoxValue>> = LazyLock::new(|| {
    HashMap::from([(
        "clock".to_string(),
        LoxValue::Callable {
            name: "clock".to_string(),
            arity: 0,
            function: |_, _| {
                Ok(LoxValue::Number(OrderedFloat::from(
                    std::time::SystemTime::now()
                        .duration_since(std::time::SystemTime::UNIX_EPOCH)
                        .unwrap()
                        .as_secs_f64(),
                )))
            },
        },
    )])
});
