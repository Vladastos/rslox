mod yoloscript;
use std::path::PathBuf;

use clap::Parser;
use yoloscript::YoloError;

#[derive(Parser)]
#[command(version, about, long_about = None, author)]
struct Args {
    /// The script to run
    script: Option<PathBuf>,
}

fn main() -> Result<(), YoloError> {
    // Set up logging
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("trace"));

    let mut yolo = yoloscript::Yolo::new();

    match Args::parse() {
        Args {
            script: Some(script),
        } => yolo.run_file(&script)?,
        Args { script: None } => yolo.run_prompt()?,
    }

    Ok(())
}
