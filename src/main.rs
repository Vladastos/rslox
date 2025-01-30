mod rslox;
use rslox::LoxError;

fn main() -> Result<(), LoxError> {
    // Set up logging
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("trace"));

    let mut lox = rslox::Lox::new();
    let args: Vec<String> = std::env::args().collect();

    if args.len() > 2 {
        println!("Usage: {} [script]", args[0]);
        return Ok(());
    } else if args.len() == 2 {
        lox.run_file(&args[1])?;
    } else {
        lox.run_prompt()?;
    }

    Ok(())
}
