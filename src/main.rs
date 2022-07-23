use std::{env::args, error::Error};

use error::{error_at, Result};
use parser::{Expr, Stmt};
use scanner::Scanner;

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
    let p = parser::Parser::new(tokens.into_iter().peekable()).stmts()?;

    println!("  .global main");
    println!("main:");

    // alloc stack
    // todo: store a3?
    println!("addi a3, sp, 0"); // mv a3, sp
    println!("addi sp, sp, -208"); // 208 == ('z' - 'a' + 1) * 8, it's the stack size for all possible single-letter 64 bit integer variables.
    
    stmts(p)?;

    // free stack
    println!("addi sp, a3, 0");
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

fn gen_expr(expr: Expr) -> Result<()> {
    match expr {
        Expr::Number(n) => {
            println!("  li a0, {}", n);
            Ok(())
        }
        Expr::Unay { op, expr } => {
            gen_expr(*expr)?;
            match op {
                parser::BinOp::SUB => {
                    println!("  sub a0, x0, a0");
                }
                parser::BinOp::ADD => {}
                _ => unreachable!(),
            }
            Ok(())
        }
        Expr::Binary { op, lhs, rhs } => {
            gen_expr(*lhs)?;
            push();
            gen_expr(*rhs)?;
            pop("a1");

            use parser::BinOp::*;
            match op {
                ADD => println!("  add a0, a1, a0"),
                SUB => println!("  sub a0, a1, a0"),
                MUL => println!("  mul a0, a1, a0"),
                DIV => println!("  div a0, a1, a0"),
                LT => println!("  slt a0, a1, a0"),
                LE => {
                    println!("  sub a0, a1, a0");
                    println!("  slti a0, a0, 1");
                }
                EQ => {
                    println!("  sub a0, a1, a0");
                    println!("  sltiu a0, a0, 1");
                }
                NE => {
                    println!("  sub a0, a1, a0");
                    println!("  sltu a0, x0, a0");
                }
            };

            Ok(())
        }
        Expr::Ident(_) => {
            gen_addr(expr)?;
            println!("  ld a0, (a0)");
            Ok(())
        }
        Expr::Assign { lhs, rhs } => {
            gen_addr(*lhs)?;
            push();
            gen_expr(*rhs)?;
            pop("a1");
            println!("sd a0, (a1)");
            Ok(())
        }
    }
}

fn stmts(ss: Vec<Stmt>) -> Result<()> {
    for s in ss {
        stmt(s)?;
    }
    Ok(())
}

fn stmt(s: Stmt) -> Result<()> {
    match s {
        Stmt::Expr(expr) => gen_expr(*expr),
    }
}

fn gen_addr(e: Expr) -> Result<()> {
    match e {
        Expr::Ident(name) => {
            if name.len() != 1 {
                todo!()
            }
            let offset = (name.chars().nth(0).unwrap() as u8 - 'a' as u8 + 1) * 8;
            println!("  addi a0, a3, -{}", offset);
            Ok(())
        }
        _ => todo!(),
    }
}
