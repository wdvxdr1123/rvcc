use crate::error::Result;
use crate::parser::{self, Expr, Stmt};

#[derive(Default)]
pub struct Func {
    pub body: Vec<Stmt>,
    pub objs: Vec<Object>,
    pub stack_size: usize,
}

pub struct Object {
    pub name: String,
    pub offset: usize,
}

pub struct Compiler {
    pub func: Func,
}

fn push() {
    println!("  addi sp, sp, -8");
    println!("  sd   a0, 0(sp)");
}

fn pop(reg: &str) {
    println!("  ld   {}, 0(sp)", reg);
    println!("  addi sp, sp, 8");
}

fn align_to(n: usize, align: usize) -> usize {
    (n + align - 1) / align * align
}

impl Compiler {
    pub fn compile(&mut self) -> Result<()> {
        self.compute_lval_offset();

        println!("  .global main");
        println!("main:");
        // alloc stack
        // todo: store a3?
        println!("  addi a3, sp, 0"); // mv a3, sp
        println!("  addi sp, sp, -{}", self.func.stack_size);

        for s in self.func.body.clone().into_iter() {
            self.stmt(s)?;
        }

        println!(".L.return:");
        // free stack
        println!("  addi sp, a3, 0");
        println!("  ret");
        Ok(())
    }

    fn compute_lval_offset(&mut self) {
        let mut offset = 0;
        for o in self.func.objs.iter_mut() {
            o.offset = offset;
            offset = offset + 8;
        }
        self.func.stack_size = align_to(offset, 16);
    }

    fn gen_expr(&mut self, expr: Expr) -> Result<()> {
        match expr {
            Expr::Number(n) => {
                println!("  li a0, {}", n);
                Ok(())
            }
            Expr::Unay { op, expr } => {
                self.gen_expr(*expr)?;
                match op {
                    parser::BinOp::SUB => {
                        // a0 = 0 - a0
                        println!("  sub a0, x0, a0");
                    }
                    parser::BinOp::ADD => {}
                    _ => unreachable!(),
                }
                Ok(())
            }
            Expr::Binary { op, lhs, rhs } => {
                self.gen_expr(*lhs)?;
                push();
                self.gen_expr(*rhs)?;
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
                self.gen_addr(expr)?;
                println!("  ld a0, (a0)");
                Ok(())
            }
            Expr::Assign { lhs, rhs } => {
                self.gen_addr(*lhs)?;
                push();
                self.gen_expr(*rhs)?;
                pop("a1");
                println!("sd a0, (a1)");
                Ok(())
            }
        }
    }

    fn stmts(&mut self, ss: Vec<Stmt>) -> Result<()> {
        for s in ss {
            self.stmt(s)?;
        }
        Ok(())
    }

    fn stmt(&mut self, s: Stmt) -> Result<()> {
        match s {
            Stmt::Expr(expr) => self.gen_expr(*expr),
            Stmt::Return(expr) => {
                self.gen_expr(*expr)?;
                println!("  j .L.return");
                Ok(())
            },
        }
    }

    fn gen_addr(&mut self, e: Expr) -> Result<()> {
        match e {
            Expr::Ident(name) => {
                for i in self.func.objs.iter() {
                    if i.name == name {
                        println!("  addi a0, a3, -{}", i.offset);
                    }
                }
                Ok(())
            }
            _ => todo!(),
        }
    }
}
