use std::{env::args, error::Error};

use error::{error_at, Result};
use scanner::Scanner;

mod compiler;
mod error;
mod parser;
mod position;
mod scanner;

const DEBUG: bool = false;

fn main() -> std::result::Result<(), Box<dyn Error>> {
    let args = args().collect::<Vec<String>>();

    if args.len() != 2 {
        eprintln!("{}: invalid number of arguments", args.get(0).unwrap());
        return Ok(());
    }

    compile(args.get(1).unwrap().to_string()).unwrap_or_else(|e| {
        error_at(args.get(1).unwrap().to_owned(), e.pos, e.msg);
        ()
    });

    Ok(())
}

fn compile(s: String) -> Result<()> {
    let mut s = Scanner::new(s.chars().peekable());
    let tokens = s.scan()?;
    if DEBUG {
        // print token stream in debug mode
        for tok in tokens.iter() {
            println!("{}", tok.kind);
        }
    }
    let prog = parser::Parser::new(tokens.into_iter().peekable()).function()?;
    compiler::Compiler { func: prog, if_count: 0 }.compile()?;
    Ok(())
}
