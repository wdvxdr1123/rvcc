use std::{env::args, error::Error};

use crate::tokenize::Token;

mod tokenize;
mod error;

fn main() -> Result<(), Box<dyn Error>> {
    let args = args().collect::<Vec<String>>();

    if args.len() != 2 {
        eprintln!("{}: invalid number of arguments", args.get(0).unwrap());
        return Ok(());
    }

    let mut tokenizer = tokenize::Tokenizer::new(args.get(1).unwrap().chars().peekable());

    println!("  .global main");
    println!("main:");
    println!("  li a0, {}", tokenizer.expect_num()?);

    while let Some(tok) = tokenizer.next() {
        match tok {
            Token::Punct('+') => {
                println!("  addi a0, a0, {}", tokenizer.expect_num()?);
            }
            Token::Punct('-') => {
                println!("  addi a0, a0, -{}", tokenizer.expect_num()?);
            }
            _ => {
                eprintln!("unexpected token: {}", tok);
                return Ok(());
            }
        }
    }

    println!("  ret");
    Ok(())
}
