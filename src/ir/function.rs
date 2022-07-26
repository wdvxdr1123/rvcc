use std::fmt;

use super::{Block, Inst};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct LocalVariable {
    pub name: String,
    pub offset: isize,
}

/// function
#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct Function {
    /// Function name
    pub name: String,

    /// Labelled blocks
    pub blocks: Vec<Block>,

    /// local variables
    pub values: Vec<LocalVariable>,

    /// stack size
    pub stack_size: isize,
}

fn align_to(n: isize, align: isize) -> isize {
    (n + align - 1) / align * align
}

impl Function {
    /// Instantiates an empty function and returns it
    pub fn new(name: String) -> Self {
        Function {
            name,
            ..Default::default()
        }
    }

    /// Adds a new empty block with a specified label and returns a reference to it
    pub fn add_block(&mut self, label: String) -> &mut Block {
        self.blocks.push(Block {
            label,
            statements: Vec::new(),
        });
        self.blocks.last_mut().unwrap()
    }

    /// Adds a new instruction to the last block
    pub fn add_instr(&mut self, inst: Inst) {
        self.blocks
            .last_mut()
            .expect("last block must be present")
            .add_instr(inst);
    }

    pub fn compute_lval_offset(&mut self) {
        let mut offset = 0;
        for o in self.values.iter_mut() {
            offset = offset + 8;
            o.offset = offset;
        }
        self.stack_size = align_to(offset, 16);
    }

    pub fn local_variable(&mut self, name: String) {
        self.values.push(LocalVariable {
            name,
            ..Default::default()
        })
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, " ${name}() {{", name = self.name)?;

        for blk in self.blocks.iter() {
            writeln!(f, "{}", blk)?;
        }

        write!(f, "}}")
    }
}
