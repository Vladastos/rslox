mod rslox;
use std::path::PathBuf;

use clap::Parser;
use rslox::LoxError;

#[derive(Parser)]
#[command(version, about, long_about = None, author)]
struct Args {
    /// The script to run
    script: Option<PathBuf>,
}

fn main() -> Result<(), LoxError> {
    // Set up logging
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("trace"));

    let mut lox = rslox::Lox::new();

    match Args::parse() {
        Args {
            script: Some(script),
        } => lox.run_file(&script)?,
        Args { script: None } => lox.run_prompt()?,
    }

    Ok(())
}
