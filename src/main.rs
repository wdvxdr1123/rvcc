use std::env::args;

use crate::tokenize::Token;

mod tokenize;

fn main() {
    let args = args().collect::<Vec<String>>();

    if args.len() != 2 {
        eprintln!("{}: invalid number of arguments", args.get(0).unwrap());
        return;
    }

    let mut tokens = tokenize::Tokenizer::new(args.get(1).unwrap().chars().peekable());

    println!("  .global main");
    println!("main:");

    if let Token::Num(i) = tokens.next().unwrap() {
        println!("  li a0, {}", i);
    } else {
        eprintln!("bad token");
        return;
    }

    while let Some(ch) = tokens.next() {
        match ch {
            Token::Punct('+') => {
                println!("  addi a0, a0, {}", tokens.next().unwrap().num().unwrap());
            }
            Token::Punct('-') => {
                println!("  addi a0, a0, -{}", tokens.next().unwrap().num().unwrap());
            }
            _ => {
                eprintln!("unexpected character:");
                return;
            }
        }
    }

    println!("  ret");
}
