use std::{env::args, error::Error};

use error::{error_at, Result};
use parser::Node;
use scanner::Scanner;

mod error;
mod parser;
mod position;
mod scanner;

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
    let tokens = s.scan()?.into_iter().peekable();
    let p = parser::Parser::new(tokens).expr()?;

    println!("  .global main");
    println!("main:");
    gen_expr(p)?;
    println!("  ret");
    Ok(())
}

fn push() {
    println!("  addi sp, sp, -8");
    println!("  sd   a0, 0(sp)");
}

fn pop(reg: &str) {
    println!("  ld   {}, 0(sp)", reg);
    println!("  addi sp, sp, 8");
}

fn gen_expr(expr: Node) -> Result<()> {
    match expr {
        Node::Number(n) => {
            println!("  li a0, {}", n);
            Ok(())
        }
        Node::Unay { op, expr } => {
            gen_expr(*expr)?;
            match op {
                parser::BinaryOperator::SUB => {
                    println!("  sub a0, x0, a0");
                }
                parser::BinaryOperator::ADD => {}
                _ => unreachable!(),
            }
            Ok(())
        }
        Node::Binary { op, lhs, rhs } => {
            gen_expr(*lhs)?;
            push();
            gen_expr(*rhs)?;
            pop("a1");

            use parser::BinaryOperator::*;
            match op {
                ADD => println!("  add a0, a1, a0"),
                SUB => println!("  sub a0, a1, a0"),
                MUL => println!("  mul a0, a1, a0"),
                DIV => println!("  div a0, a1, a0"),
                LT => println!("  slt a0, a1, a0"),
                LE => {
                    println!("  sub a0, a1, a0");
                    println!("  slti a0, a0, 1");
                },
                EQ => {
                    println!("  sub a0, a1, a0");
                    println!("  sltiu a0, a0, 1");
                },
                NE => {
                    println!("  sub a0, a1, a0");
                    println!("  sltu a0, x0, a0");
                },
            };

            Ok(())
        }
    }
}
