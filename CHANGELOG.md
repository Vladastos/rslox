# Changelog

## 0.0.9

### New Features

Changed the way variables are stored in the interpreter, now it's a `HashMap<String, Rc<RefCell<LoxValue>>>`. This allows for variables to be captured in functions and used in the function body. This also allows for mutable references to variables, which was impossible before.

#### Clojure Functions





