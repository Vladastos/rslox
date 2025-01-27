mod rslox;
use rslox::LoxError;

fn main() {
    // Set up logging
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("trace"));

    let mut lox = rslox::Lox::new();
    let args: Vec<String> = std::env::args().collect();

    let result: Result<(), LoxError>;
    if args.len() > 2 {
        println!("Usage: {} [script]", args[0]);
        return;
    } else if args.len() == 2 {
        result = lox.run_file(&args[1]);
    } else {
        result = lox.run_prompt();
    }

    if result.is_err() {
        println!("{}", result.unwrap_err());
    }
}
