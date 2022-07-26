use crate::ir::{self, Function};

#[derive(Debug)]
pub struct CodegenContext<'a> {
    func: &'a ir::Function,
}

impl<'a> CodegenContext<'a> {
    pub fn new(func: &'a Function) -> Self {
        CodegenContext { func }
    }

    pub fn codegen(&self) {
        println!("  .globl {}", self.func.name);
        println!("{}:", self.func.name);

        // 压入 fp
        println!("  addi sp, sp, -16");
        println!("  sd fp, 0(sp)");
        println!("  sd ra, 8(sp)");
        println!("  mv fp, sp");

        // stack alloc for local variables
        println!("  addi sp, sp, -{}", self.func.stack_size);

        for blk in self.func.blocks.iter() {
            self.gen_block(blk);
        }

        println!(".L.return:");
        println!("  mv sp, fp");
        println!("  ld fp, 0(sp)");
        println!("  ld ra, 8(sp)");
        println!("  addi sp, sp, 16");
        println!("  ret");
    }

    fn gen_block(&self, blk: &ir::Block) {
        println!("{}:", blk.label);
        for inst in blk.statements.iter() {
            self.gen_inst(inst);
        }
    }

    fn gen_inst(&self, inst: &ir::Inst) {
        match inst {
            ir::Inst::Add => println!("  add a0, a0, a1"),
            ir::Inst::Sub => println!("  sub a0, a0, a1"),
            ir::Inst::Mul => println!("  mul a0, a0, a1"),
            ir::Inst::Div => println!("  div a0, a0, a1"),
            ir::Inst::Rem => todo!(),
            ir::Inst::Cmp(cmp) => match cmp {
                ir::Cmp::LT => println!("  slt a0, a0, a1"),
                ir::Cmp::LE => {
                    println!("  sub a0, a0, a1");
                    println!("  slti a0, a0, 1");
                }
                ir::Cmp::EQ => {
                    println!("  xor a0, a0, a1");
                    println!("  sltiu a0, a0, 1");
                }
                ir::Cmp::NE => {
                    println!("  xor a0, a0, a1");
                    println!("  sltu a0, x0, a0");
                }
            },
            ir::Inst::And => todo!(),
            ir::Inst::Or => todo!(),
            ir::Inst::Ret => println!("  j .L.return"),
            ir::Inst::Jz(label) => println!("  beqz a0, {}", label),
            ir::Inst::Jmp(label) => println!("  j {}", label),
            ir::Inst::Push => {
                println!("  addi sp, sp, -8");
                println!("  sd a0, 0(sp)");
            }
            ir::Inst::Pop(reg) => {
                println!("  ld {}, 0(sp)", reg);
                println!("  addi sp, sp, 8");
            }
            ir::Inst::Imm(imm) => println!("  li a0, {}", imm),
            ir::Inst::Neg => println!("  sub a0, x0, a0"), // -a = 0-a
            ir::Inst::Load => println!("  ld a0, (a0)"),
            ir::Inst::Store(to) => println!("  sd a0, ({})", to),
            ir::Inst::Shl(i) => println!("  slli a0, a0, {}", i),
            ir::Inst::Shr(i) => println!("  srli a0, a0, {}", i),
            ir::Inst::LocalVariable(name) => {
                for var in self.func.values.iter() {
                    if *name == var.name {
                        println!("  addi a0, fp, {}", var.offset - self.func.stack_size);
                        break;
                    }
                }
            }
            ir::Inst::Call(name) => {
                println!("  li a0, 0");
                println!("  call {}", name);
            }
        }
    }
}
