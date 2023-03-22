use crate::ir::Type;
use std::ops::Index;
use std::fmt;

pub use primitive::Word;

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
        self.data.len().into()
    }
}

// impl Index<Word> for Stack {
//     type Output = Word;
// 
//     fn index(&self, idx: Word) -> &Word {
//         &self.data[Into::<usize>::into(idx)]
//     }
// }

struct Heap {
    freed: Vec<usize>,
    blocks: Vec<Vec<Word>>,
}

impl Heap {
    fn new() -> Self {
        Self {
            freed: vec![],
            blocks: vec![vec![]],
        }
    }

    fn alloc(&mut self, size: usize) -> Word {
        match self.freed.pop() {
            Some(i) => i.into(),
            None => {
                let b = vec![Word(0); size];
                let ret = self.blocks.len().into();
                self.blocks.push(b);
                ret
            }
        }
    }

    fn dealloc(&mut self, ptr: Word) {
        let i: usize = ptr.into();
        assert_ne!(i, 0);
        self.blocks[i].clear();
        self.freed.push(i);
    }

    fn read(&self, ptr: Word, offset: usize) -> Word {
        let i: usize = ptr.into();
        assert_ne!(i, 0);
        self.blocks[i][offset]
    }

    fn write(&mut self, ptr: Word, offset: usize, data: Word) {
        let i: usize = ptr.into();
        assert_ne!(i, 0);
        self.blocks[i][offset] = data;
    }

    fn release(&mut self) {
        self.freed.clear();
        self.blocks.truncate(1);
    }

    fn clone(&mut self, ptr: Word) -> Word {
        let i: usize = ptr.into();
        assert_ne!(i, 0);
        let b = self.blocks[i].clone();
        let ret = self.blocks.len().into();
        self.blocks.push(b);
        ret
    }

    fn push(&mut self, ptr: Word, v: Word) {
        let i: usize = ptr.into();
        assert_ne!(i, 0);
        self.blocks[i].push(v)
    }

    fn pop(&mut self, ptr: Word) {
        let i: usize = ptr.into();
        assert_ne!(i, 0);
        let _ = self.blocks[i].pop();
    }
}

pub enum Instr {
    Halt,
    Apply,
    TailApply,
    Return,
    Grab,
    Let,
    EndLet,
    MkClosure(Word),
    LFJF(Word),
    Load(Word),
    PushMark,
    PushA,
    Fetch(Word),

    Add,
    Sub,
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instr::Halt => write!(f, "halt"),
            Instr::Apply => write!(f, "apply"),
            Instr::TailApply => write!(f, "tail_apply"),
            Instr::Grab => write!(f, "grab"),
            Instr::Let=> write!(f, "let"),
            Instr::EndLet=> write!(f, "end_let"),
            Instr::MkClosure(Word(n)) => write!(f, "make_closure {}", n),
            Instr::LFJF(Word(n)) => write!(f, "local_function_and_jump_forward {}", n),
            Instr::Return => write!(f, "return"),
            Instr::Load(Word(n)) => write!(f, "load {}", n),
            Instr::PushMark => write!(f, "push_mark"),
            Instr::PushA => write!(f, "pusha"),
            Instr::Fetch(Word(n)) => write!(f, "fetch {}", n),
            Instr::Add => write!(f, "add"),
            Instr::Sub=> write!(f, "sub"),
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
        addr.into()
    }
}

impl Index<Word> for Program {
    type Output = Instr;

    fn index(&self, idx: Word) -> &Instr {
        &self.data[Into::<usize>::into(idx)]
    }
}

pub struct VM {
    stack: Stack,
    heap: Heap,
    a: Word,
    pc: Word,
    env: Word,
    arg_stack: Vec<Word>,
}

impl VM {
    pub fn new() -> VM {
        let mut heap = Heap::new();
        let env = heap.alloc(0);
        VM {
            stack: Stack::new(),
            heap,
            a: Word(0),
            pc: Word(0),
            env,
            arg_stack: vec![],
        }
    }

    pub fn run(&mut self, prog: &Program, entry_point: Word) -> Word {
        self.pc = entry_point;
        loop {
            let ins = &prog[self.pc];
            match *ins {
                Instr::Halt => {
                    return self.a;
                },
                Instr::Apply => {
                    self.stack.push(self.env);
                    self.stack.push(self.pc + Word(1));
                    self.tail_apply()
                },
                Instr::TailApply => self.tail_apply(),
                Instr::Return => match self.arg_stack.pop() {
                    None | Some(Word(0)) => {
                        self.pc = self.stack.pop();
                        self.env = self.stack.pop();
                    },
                    Some(v) => {
                        let env = self.heap.read(self.a, 1);
                        self.env = self.heap.clone(env);
                        self.heap.push(self.env, v);
                        self.pc = self.heap.read(self.a, 0);
                    },
                },
                Instr::Grab => match self.arg_stack.pop() {
                    None | Some(Word(0)) => {
                        let addr = self.pc + Word(1);
                        // Is clone of env necessary?
                        self.a = self.make_closure(addr, self.env);
                        self.pc = self.stack.pop();
                        self.env = self.stack.pop();
                    }
                    Some(v) => {
                        self.heap.push(self.env, v);
                        self.pc.add1();
                    },
                },
                Instr::Let => {
                    self.heap.push(self.env, self.a);
                    self.pc.add1();
                },
                Instr::EndLet => {
                    self.heap.pop(self.env);
                    self.pc.add1();
                },
                Instr::MkClosure(addr) => {
                    let empty_env = self.heap.alloc(0);
                    self.a = self.make_closure(addr, empty_env);
                    self.pc.add1();
                },
                Instr::LFJF(offset) => {
                    let addr = self.pc + Word(1);
                    let env = self.heap.clone(self.env);
                    self.a = self.make_closure(addr, env);
                    self.pc = self.pc + offset;
                },
                Instr::Load(v) => {
                    self.a = v;
                    self.pc.add1();
                },
                Instr::PushMark => {
                    self.arg_stack.push(Word(0));
                    self.pc.add1();
                }
                Instr::PushA => {
                    self.arg_stack.push(self.a);
                    self.pc.add1();
                },
                Instr::Fetch(n) => {
                    let offset = Into::<usize>::into(n);
                    self.a = self.heap.read(self.env, offset);
                    self.pc.add1();
                },

                Instr::Add => self.prim2(primitive::add),
                Instr::Sub => self.prim2(primitive::sub),
            }
        }
    }

    fn prim2(&mut self, f: fn(Word, Word) -> Word) {
        let x = self.arg_stack.pop().unwrap();
        self.a = f(self.a, x);
        self.pc.add1();
    }

    fn tail_apply(&mut self) {
        let env = self.heap.read(self.a, 1);
        self.env = self.heap.clone(env);
        let x = self.arg_stack.pop()
            .expect("There should be at least one element in arg_stack when apply");
        self.heap.push(self.env, x);
        self.pc = self.heap.read(self.a, 0);
    }

    fn make_closure(&mut self, pc: Word, env: Word) -> Word {
        let ptr = self.heap.alloc(2);
        self.heap.write(ptr, 0, pc);
        self.heap.write(ptr, 1, env);
        ptr
    }

    pub fn print(&self, v: Word, t: &Type) {
        if t.is_arrow() {
            println!("Closure {:?}: {}", v, t);
        } else {
            println!("{}", Into::<i64>::into(v));
        }
    }

    pub fn free_mem(&mut self) {
        self.heap.release()
    }
}

pub mod primitive {
    use std::ops::Add;
    use std::mem::transmute;

    const UBOUND: u64 = 0x4000_0000_0000_0000;

    #[derive(Clone, Copy, Debug)]
    pub struct Word(pub u64);

    impl Word {
        pub fn add1(&mut self) {
            self.0 += 1;
        }
    }

    impl TryFrom<u64> for Word {
        type Error = ();
        fn try_from(v: u64) -> Result<Word, ()> {
            if v >= UBOUND {
                Err(())
            } else {
                Ok(Word(v * 2 + 1))
            }
        }
    }

    impl From<Word> for i64 {
        fn from(v: Word) -> i64 {
            let x: i64 = unsafe { transmute(v.0) };
            (x - 1) / 2
        }
    }

    impl From<usize> for Word {
        fn from(v: usize) -> Word {
            Word(v as u64)
        }
    }

    impl From<Word> for usize {
        fn from(v: Word) -> usize {
            v.0 as usize
        }
    }

    impl Add<Self> for Word {
        type Output = Self;

        fn add(self, rhs: Word) -> Word {
            Word(self.0 + rhs.0)
        }
    }

    pub fn add(x: Word, y: Word) -> Word {
        unsafe {
            let x: i64 = transmute(x.0);
            let y: i64 = transmute(y.0);
            Word(transmute(x + y - 1))
        }
    }

    pub fn sub(x: Word, y: Word) -> Word {
        unsafe {
            let x: i64 = transmute(x.0);
            let y: i64 = transmute(y.0);
            Word(transmute(x - y + 1))
        }
    }
}
