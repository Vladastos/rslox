//! TODO:
//!  - Add more built-in functions
//!
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use ordered_float::OrderedFloat;

use super::interpreter::{LoxValue, LoxValueType};

pub fn init_builtins() -> HashMap<String, Rc<RefCell<LoxValueType>>> {
    return HashMap::from([(
        "clock".to_string(),
        Rc::new(RefCell::new(LoxValueType::Constant(
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
        ))),
    )]);
}
