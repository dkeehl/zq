use crate::error::ZqError;
use crate::lexer::Lexer;
use crate::parser::{item, PState, Parser};
use crate::ast::Item;
use crate::vm::{VM, Program, Word, Instr};
use crate::ir::{NameTable, Type, Gamma,};

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

pub fn run_repl() {
    let mut rl = DefaultEditor::new().unwrap();
    let mut interp = Interpretor::new();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let res = interp.process(line);
                println!("{:?}", res);
            },
            Err(ReadlineError::Interrupted) => {
                println!("Interrupted.");
                break
            },
            Err(ReadlineError::Eof) => {
                println!("Bye!");
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }
}

struct Interpretor {
    vm: VM,
    prog: Program,
    global: NameTable<(Type, Type)>,
    name_table: NameTable<Word>,
    buffer: Vec<Instr>,
}

impl Interpretor {
    fn new() -> Self {
        Interpretor {
            vm: VM::new(),
            prog: Program::new(),
            global: NameTable::new(),
            name_table: NameTable::new(),
            buffer: vec![],
        }
    }

    fn process(&mut self, input: String) -> Result<(), ZqError> {
        let mut lexer = Lexer::new(&input[..]);
        lexer.scan()?;
        if lexer.is_empty() {
            return Ok(());
        }
        let parser = item();
        let mut state = PState::new();
        match parser.parse(&mut lexer, &mut state)? {
            Item::Expression(e) => {
                let e = e.de_bruijn_index();
                let mut gamma = Gamma::new();
                let _ = e.typing(&mut gamma, &self.global)?;
                e.compile(&self.name_table, &mut self.buffer);
                self.buffer.push(Instr::Halt);
                let addr = self.prog.append(&mut self.buffer);
                let res = self.vm.run(&self.prog, addr);
                println!("{:?}", res);
            },
            Item::FunDef(f) => {
                let f = f.de_bruijn_index();
                let mut gamma = Gamma::new();
                f.type_check(&mut gamma, &self.global)?;
                self.global.insert(f.name.clone(), f.ty.clone());
                f.compile(&self.name_table, &mut self.buffer);
                let addr = self.prog.append(&mut self.buffer);
                self.name_table.insert(f.name.clone(), addr);
            },
        }
        Ok(())
    }
}

