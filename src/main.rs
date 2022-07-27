use std::{env::args, error::Error};

use error::{error_at, Result};
use scanner::Scanner;

mod codegen;
mod error;
mod ir;
mod irgen;
mod parser;
mod position;
mod scanner;
mod typecheck;

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
    let mut p = parser::Parser::new(tokens.into_iter().peekable());

    let mut decls = vec![];
    while !p.end() {
        decls.push(p.decl()?);
    }

    irgen::IRGen::new(decls).compile()?;
    Ok(())
}
