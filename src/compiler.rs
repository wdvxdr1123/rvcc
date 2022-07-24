use std::fmt;

use crate::error::{Result, SyntaxError};
use crate::parser::{self, Expr, Node, Stmt, UnaryOp};
use crate::position::Position;

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
    label_index: usize,
    pos: Position,
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
    pub fn new(func: Func) -> Self {
        Compiler {
            func,
            label_index: 0,
            pos: Position::no_postion(),
        }
    }

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
            offset = offset + 8;
            o.offset = offset;
        }
        self.func.stack_size = align_to(offset, 16);
    }

    fn count(&mut self) -> usize {
        let c = self.label_index;
        self.label_index = c + 1;
        c
    }

    fn set_position<T: Node>(&mut self, n: &T) {
        self.pos = n.pos();
    }

    fn gen_expr(&mut self, expr: Expr) -> Result<()> {
        self.set_position(&expr);
        match expr {
            Expr::Number { value, .. } => {
                println!("  li a0, {}", value);
                Ok(())
            }
            Expr::Unary {
                op, expr: _expr, ..
            } => {
                if op == UnaryOp::ADDR {
                    return self.gen_addr(*_expr);
                }
                self.gen_expr(*_expr)?;
                match op {
                    UnaryOp::POSITIVE => {}
                    UnaryOp::NEGATIVE => {
                        // a0 = 0 - a0
                        println!("  sub a0, x0, a0");
                    }
                    UnaryOp::DEREF => {
                        println!("  ld a0, (a0)");
                    }
                    _ => self.error("invalid unary operator")?,
                }
                Ok(())
            }
            Expr::Binary { op, lhs, rhs, .. } => {
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
            Expr::Ident { .. } => {
                self.gen_addr(expr)?;
                println!("  ld a0, (a0)");
                Ok(())
            }
            Expr::Assign { lhs, rhs, .. } => {
                self.gen_addr(*lhs)?;
                push();
                self.gen_expr(*rhs)?;
                pop("a1");
                println!("  sd a0, (a1)");
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
        self.set_position(&s);
        match s {
            Stmt::Expr { expr, .. } => self.gen_expr(*expr),
            Stmt::Return { expr, .. } => {
                self.gen_expr(*expr)?;
                println!("  j .L.return");
                Ok(())
            }
            Stmt::Block { body, .. } => self.stmts(body),
            Stmt::None(_) => Ok(()),
            Stmt::If {
                cond, then, r#else, ..
            } => {
                let c = self.count();

                self.gen_expr(*cond)?;
                println!("  beqz a0, .L.else.{}", c);
                self.stmt(*then)?;
                println!("  j .L.end.{}", c);
                println!(".L.else.{}:", c);
                if let Some(els) = r#else {
                    self.stmt(*els)?;
                }
                println!(".L.end.{}:", c);
                Ok(())
            }
            Stmt::For {
                init,
                cond,
                post,
                body,
                ..
            } => {
                let c = self.count();
                // while statement does not have init
                if let Some(init) = init {
                    self.stmt(*init)?;
                }
                println!(".L.start.{}:", c);
                if let Some(cond) = cond {
                    self.gen_expr(*cond)?;
                    println!("  beqz a0, .L.end.{}", c);
                }

                self.stmt(*body)?;

                if let Some(post) = post {
                    self.gen_expr(*post)?;
                }

                println!("  j .L.start.{}", c);
                println!(".L.end.{}:", c);
                Ok(())
            }
        }
    }

    fn gen_addr(&mut self, e: Expr) -> Result<()> {
        self.set_position(&e);
        match e {
            Expr::Ident { name, .. } => {
                for i in self.func.objs.iter() {
                    if i.name == name {
                        println!("  addi a0, a3, {}", i.offset as isize - self.func.stack_size as isize);
                    }
                }
                Ok(())
            },
            Expr::Unary { op,expr,..} => {
                if op != UnaryOp::DEREF {
                    return self.error("is not addressable");
                }
                self.gen_expr(*expr)
            }
            _ => self.error("is not addressable"),
        }
    }

    fn error<T, V: fmt::Display>(&self, msg: V) -> Result<T> {
        Err(SyntaxError {
            pos: self.pos,
            msg: msg.to_string(),
        })
    }
}
