use std::env;
use std::fs;
use std::process;

use agi_cc::Target;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: agi-cc <source.c>");
        process::exit(1);
    }

    let source = match fs::read_to_string(&args[1]) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: could not read {}: {e}", args[1]);
            process::exit(1);
        }
    };

    let target = if cfg!(target_os = "macos") {
        Target::MacOS
    } else {
        Target::Linux
    };

    match agi_cc::compile_for_target(&source, target) {
        Ok(asm) => print!("{asm}"),
        Err(e) => {
            eprintln!("{e}");
            process::exit(1);
        }
    }
}
