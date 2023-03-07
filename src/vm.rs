use std::ops::{Index, Add};
use std::fmt;

pub struct VM {
    stack: Stack,
    a: Word,
    pc: Word,
    frame_pointer: Word,
}

#[derive(Clone, Copy, Debug)]
pub struct Word(pub u64);

impl Word {
    fn add1(&mut self) {
        self.0 += 1;
    }
}

impl Add<Self> for Word {
    type Output = Self;

    fn add(self, rhs: Word) -> Word {
        Word(self.0 + rhs.0)
    }
}

struct Stack {
    data: Vec<Word>,
}

impl Stack {
    fn new() -> Self {
        Stack {
            data: vec![Word(0)]
        }
    }

    fn push(&mut self, v: Word) {
        self.data.push(v);
    }

    fn pop(&mut self) -> Word {
        self.data.pop().unwrap()
    }

    fn sp(&self) -> Word {
        let p = self.data.len() as u64;
        Word(p)
    }
}

impl Index<Word> for Stack {
    type Output = Word;

    fn index(&self, idx: Word) -> &Word {
        &self.data[idx.0 as usize]
    }
}

pub enum Instr {
    Halt,
    Call,
    Return,
    Load(Word),
    PushA,
    Push(Word),
    Pop,
    Fetch(Word),
    Add,
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instr::Halt => write!(f, "halt"),
            Instr::Call => write!(f, "call"),
            Instr::Return => write!(f, "return"),
            Instr::Load(Word(n)) => write!(f, "load {}", n),
            Instr::PushA => write!(f, "pusha"),
            Instr::Push(Word(n)) => write!(f, "push {}", n),
            Instr::Pop => write!(f, "pop"),
            Instr::Fetch(Word(n)) => write!(f, "fetch {}", n),
            Instr::Add => write!(f, "add"),
        }
    }
}

pub struct Program {
    data: Vec<Instr>
}

impl Program {
    pub fn print(&self) {
        for i in self.data.iter() {
            println!("{}", i);
        }
    }

    pub fn new() -> Self {
        Program { data: vec![] }
    }
    
    pub fn append(&mut self, ins: &mut Vec<Instr>) -> Word {
        let addr = self.data.len();
        self.data.append(ins);
        Word(addr as u64)
    }
}

impl Index<Word> for Program {
    type Output = Instr;

    fn index(&self, idx: Word) -> &Instr {
        &self.data[idx.0 as usize]
    }
}

impl VM {
    pub fn new() -> VM {
        VM {
            stack: Stack::new(),
            a: Word(0),
            pc: Word(0),
            frame_pointer: Word(0),
        }
    }

    pub fn run(&mut self, prog: &Program, entry_point: Word) -> Word {
        self.pc = entry_point;
        loop {
            let ins = &prog[self.pc];
            match *ins {
                Instr::Halt => return self.a,
                Instr::Call => {
                    let f = self.stack.pop();
                    self.stack.push(self.frame_pointer);
                    self.frame_pointer = self.stack.sp();
                    self.stack.push(self.pc + Word(1));
                    self.pc = f;
                },
                Instr::Return => {
                    self.pc = self.stack.pop();
                    self.frame_pointer = self.stack.pop();
                },
                Instr::Load(v) => {
                    self.a = v;
                    self.pc.add1();
                },
                Instr::Push(v) => {
                    self.stack.push(v);
                    self.pc.add1();
                },
                Instr::PushA => {
                    self.stack.push(self.a);
                    self.pc.add1();
                },
                Instr::Pop => {
                    self.stack.pop();
                    self.pc.add1();
                },
                Instr::Fetch(n) => {
                    let idx = self.frame_pointer + n;
                    self.a = self.stack[idx];
                    self.pc.add1();
                },

                Instr::Add => {
                    let x = self.stack.pop();
                    self.a.0 += x.0;
                    self.pc.add1();
                },
            }
        }
    }
}

