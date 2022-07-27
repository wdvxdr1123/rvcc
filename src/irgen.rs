use std::collections::HashMap;
use std::fmt;

use crate::codegen;
use crate::error::{Result, SyntaxError};
use crate::ir::{self, Cmp, Inst};
use crate::parser::{BinOp, Decl, Expr, Node, Stmt, UnaryOp};
use crate::position::Position;
use crate::typecheck::{Kind, Type};

const ARGUMENT_REGS: [&str; 8] = ["a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"];

pub struct IRGen {
    label_index: usize,
    pos: Position,
    decls: Vec<Decl>,

    pub functions: Vec<ir::Function>,

    ident_ty: HashMap<String, Type>,
}

impl IRGen {
    pub fn new(decls: Vec<Decl>) -> Self {
        IRGen {
            label_index: 0,
            pos: Position::no_postion(),
            decls,
            functions: vec![],
            ident_ty: HashMap::new(),
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

    pub fn compile(&mut self) -> Result<()> {
        for  ref decl in self.decls.clone().iter() {
            self.gen_decl(decl)?;
        }
        Ok(())
    }

    fn gen_decl(&mut self, decl : &Decl) -> Result<()> {
        match decl {
            Decl::Func(f) => {
                self.add_function(f.name.clone());

                let mut prog = f.body.clone();
                self.stmts(&mut prog)?;

                self.functions.last_mut().unwrap().compute_lval_offset();

                codegen::CodegenContext::new(self.functions.last().unwrap()).codegen();
            }
        }
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

    fn gen_expr(&mut self, expr: &mut Expr) -> Result<()> {
        self.set_position(expr);
        self.check_expr(expr)?;
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
            Expr::Call {
                name, arguments, ..
            } => {
                let argc = arguments.len();
                for argument in arguments {
                    self.gen_expr(argument)?;
                    self.push();
                }

                for reg in ARGUMENT_REGS.iter().take(argc).rev() {
                    self.pop(*reg);
                }

                self.add_inst(Inst::Call(name.clone()));
                Ok(())
            }
        }
    }

    fn stmts(&mut self, ss: &mut Vec<Stmt>) -> Result<()> {
        for s in ss {
            self.stmt(s)?;
        }
        Ok(())
    }

    fn stmt(&mut self, s: &mut Stmt) -> Result<()> {
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
                    self.ident_ty.insert(decl.name.clone(), decl.ty.clone());
                    if let Some(ref mut init) = decl.init {
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

    fn gen_addr(&mut self, e: &mut Expr) -> Result<()> {
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

    // type check for expression
    pub fn check_expr(&self, expr: &mut Expr) -> Result<()> {
        if expr.typecheck().checked() {
            return Ok(());
        }

        match expr {
            Expr::Binary {
                op, lhs, rhs, ty, ..
            } => {
                self.check_expr(lhs)?;
                self.check_expr(rhs)?;

                *ty = match op {
                    BinOp::SUB => {
                        let (lty, rty) = (lhs.typecheck(), rhs.typecheck());
                        if lty.is(Kind::Ptr) && rty.is(Kind::Ptr) {
                            Type::Int
                        } else {
                            lty.clone()
                        }
                    }
                    _ => lhs.typecheck().clone(),
                };
            }
            Expr::Unary { op, expr, ty, .. } => {
                self.check_expr(expr)?;
                match op {
                    UnaryOp::ADDR => {
                        *ty = expr.typecheck().pointer_to();
                    }
                    UnaryOp::DEREF => {
                        *ty = expr.typecheck().elem();
                    }
                    _ => *ty = expr.typecheck().clone(),
                }
            }
            Expr::Number { ty, .. } => {
                *ty = Type::Int;
            }
            Expr::Ident { ty, name, .. } => {
                *ty = self.ident_ty.get(name).unwrap().clone();
            }
            Expr::Assign { lhs, rhs, ty, .. } => {
                self.check_expr(rhs)?;
                self.check_expr(lhs)?;
                *ty = lhs.typecheck().clone();
            }
            Expr::Call { arguments, ty, .. } => {
                for argument in arguments {
                    self.check_expr(argument)?;
                }
                *ty = Type::Int;
            }
        }
        Ok(())
    }

    fn error<T, V: fmt::Display>(&self, msg: V) -> Result<T> {
        Err(SyntaxError {
            pos: self.pos,
            msg: msg.to_string(),
        })
    }
}
