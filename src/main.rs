use std::env::args;
use std::iter::Peekable;
use std::str::Chars;

fn main() {
    let args = args().collect::<Vec<String>>();

    if args.len() != 2 {
        eprintln!("{}: invalid number of arguments", args.get(0).unwrap());
        return;
    }

    let codes = &mut args.get(1).unwrap().chars().peekable();

    println!("  .global main");
    println!("main:");
    println!("  li a0, {}", strtol(codes));

    while let Some(ch) = codes.next() {
        match ch {
            '+' => {
                println!("  addi a0, a0, {}", strtol(codes));
            },
            '-' => {
                println!("  addi a0, a0, -{}", strtol(codes));
            },
            _ => {
                eprintln!("unexpected character: {}", ch);
                return
            }
        }
    }

    println!("  ret");
}

fn strtol(chars: &mut Peekable<Chars>) -> usize {
    let mut num: usize = 0;
    while let Some(ch) = chars.peek() {
        match ch {
            '0'..='9' => {
                num = num * 10 + *ch as usize - '0' as usize;
                chars.next();
            }
            _ => {
                break;
            }
        }
    }
    num
}
