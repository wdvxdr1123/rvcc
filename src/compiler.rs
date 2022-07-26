use std::collections::HashMap;
use std::fmt;

use crate::codegen;
use crate::error::{Result, SyntaxError};
use crate::ir::{self, Cmp, Inst};
use crate::parser::{BinOp, Expr, Node, Stmt, UnaryOp};
use crate::position::Position;
use crate::typecheck::{self, Kind, Type};

#[derive(Default)]
pub struct Func {
    pub body: Vec<Stmt>,
}

pub struct Compiler {
    pub func: Func,
    label_index: usize,
    pos: Position,

    pub functions: Vec<ir::Function>,
}

impl Compiler {
    pub fn new(func: Func) -> Self {
        Compiler {
            func,
            label_index: 0,
            pos: Position::no_postion(),
            functions: vec![],
        }
    }

    fn add_function(&mut self, name: String) -> &mut ir::Function {
        self.functions.push(ir::Function::new(name.clone()));
        let func = self.functions.last_mut().unwrap();
        func.add_block(format!(".L.entry.{}", name));
        func
    }

    fn add_block(&mut self, label: String) -> &mut ir::Block {
        self.functions.last_mut().unwrap().add_block(label)
    }

    fn add_inst(&mut self, inst: ir::Inst) {
        self.functions.last_mut().unwrap().add_instr(inst)
    }

    fn local_variable(&mut self, name: String) {
        self.functions.last_mut().unwrap().local_variable(name);
    }

    fn push(&mut self) {
        self.add_inst(Inst::Push);
    }

    fn pop(&mut self, reg: &str) {
        self.add_inst(Inst::Pop(reg.to_string()));
    }

    pub fn compile(&mut self, ty_ident: HashMap<String, Type>) -> Result<()> {
        self.add_function("main".into());

        let mut prog = self.func.body.clone();
        typecheck::Context::new(ty_ident)
            .typecheck(&mut prog)
            .unwrap();
        self.stmts(&prog)?;

        self.functions.last_mut().unwrap().compute_lval_offset();

        codegen::CodegenContext::new(self.functions.last().unwrap()).codegen();

        Ok(())
    }

    fn count(&mut self) -> usize {
        let c = self.label_index;
        self.label_index = c + 1;
        c
    }

    fn set_position<T: Node>(&mut self, n: &T) {
        self.pos = n.pos();
    }

    fn gen_expr(&mut self, expr: &Expr) -> Result<()> {
        self.set_position(expr);
        match expr {
            Expr::Number { value, .. } => {
                self.add_inst(Inst::Imm(*value));
                Ok(())
            }
            Expr::Unary { op, expr, .. } => {
                if *op == UnaryOp::ADDR {
                    return self.gen_addr(expr);
                }
                self.gen_expr(expr)?;
                match op {
                    UnaryOp::POSITIVE => {}
                    UnaryOp::NEGATIVE => self.add_inst(Inst::Neg),
                    UnaryOp::DEREF => self.add_inst(Inst::Load),
                    _ => self.error("invalid unary operator")?,
                }
                Ok(())
            }
            Expr::Binary { op, lhs, rhs, .. } => {
                let (lty, rty) = (lhs.typecheck(), rhs.typecheck());
                let ptr_index = lty.is(Kind::Ptr) || rty.is(Kind::Ptr);

                self.gen_expr(rhs)?;
                if ptr_index && rty.is(Kind::Int) && (*op == BinOp::ADD || *op == BinOp::SUB) {
                    self.add_inst(Inst::Shl(3));
                }
                self.push();

                self.gen_expr(lhs)?;
                if ptr_index && lty.is(Kind::Int) && *op == BinOp::ADD {
                    self.add_inst(Inst::Shl(3));
                }

                self.pop("a1");
                let inst = match op {
                    BinOp::ADD => Inst::Add,
                    BinOp::SUB => Inst::Sub,
                    BinOp::MUL => Inst::Mul,
                    BinOp::DIV => Inst::Div,
                    BinOp::LT => Inst::Cmp(Cmp::LT),
                    BinOp::LE => Inst::Cmp(Cmp::LE),
                    BinOp::EQ => Inst::Cmp(Cmp::EQ),
                    BinOp::NE => Inst::Cmp(Cmp::NE),
                };
                self.add_inst(inst);

                if ptr_index && rty.is(Kind::Ptr) && rty.is(Kind::Ptr) && *op == BinOp::SUB {
                    self.add_inst(Inst::Shr(3));
                }

                Ok(())
            }
            Expr::Ident { .. } => {
                self.gen_addr(expr)?;
                self.add_inst(Inst::Load);
                Ok(())
            }
            Expr::Assign { lhs, rhs, .. } => {
                self.gen_addr(lhs)?;
                self.push();

                self.gen_expr(rhs)?;
                self.pop("a1");

                self.add_inst(Inst::Store("a1".into()));
                Ok(())
            }
            Expr::Call {  name, .. } => {
                self.add_inst(Inst::Call(name.clone()));
                Ok(())
            }
        }
    }

    fn stmts(&mut self, ss: &Vec<Stmt>) -> Result<()> {
        for s in ss {
            self.stmt(s)?;
        }
        Ok(())
    }

    fn stmt(&mut self, s: &Stmt) -> Result<()> {
        self.set_position(s);
        match s {
            Stmt::Expr { expr, .. } => self.gen_expr(expr),
            Stmt::Return { expr, .. } => {
                self.gen_expr(expr)?;
                self.add_inst(Inst::Ret);
                Ok(())
            }
            Stmt::Block { body, .. } => self.stmts(body),
            Stmt::None(_) => Ok(()),
            Stmt::If {
                cond, then, r#else, ..
            } => {
                let c = self.count();

                self.gen_expr(cond)?;
                self.add_inst(Inst::Jz(format!(".L.else.{}", c)));

                self.stmt(then)?;
                self.add_inst(Inst::Jmp(format!(".L.end.{}", c)));

                self.add_block(format!(".L.else.{}", c));
                if let Some(els) = r#else {
                    self.stmt(els)?;
                }

                self.add_block(format!(".L.end.{}", c));
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
                    self.stmt(init)?;
                }

                self.add_block(format!(".L.start.{}", c));
                if let Some(cond) = cond {
                    self.gen_expr(cond)?;
                    self.add_inst(Inst::Jz(format!(".L.end.{}", c)));
                }

                self.stmt(body)?;

                if let Some(post) = post {
                    self.gen_expr(post)?;
                }
                self.add_inst(Inst::Jmp(format!(".L.start.{}", c)));

                self.add_block(format!(".L.end.{}", c));
                Ok(())
            }
            Stmt::Declaration { decls, .. } => {
                for decl in decls {
                    self.local_variable(decl.name.clone());
                    if let Some(init) = &decl.init {
                        self.add_inst(Inst::LocalVariable(decl.name.clone()));
                        self.push();
                        self.gen_expr(init)?;
                        self.pop("a1");
                        self.add_inst(Inst::Store("a1".to_string()));
                    }
                }
                Ok(())
            }
        }
    }

    fn gen_addr(&mut self, e: &Expr) -> Result<()> {
        self.set_position(e);
        match e {
            Expr::Ident { name, .. } => {
                self.add_inst(Inst::LocalVariable(name.clone()));
                Ok(())
            }
            Expr::Unary { op, expr, .. } => {
                if *op != UnaryOp::DEREF {
                    return self.error("is not addressable");
                }
                self.gen_expr(expr)
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
