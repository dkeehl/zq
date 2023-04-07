use crate::error::ZqError;
use crate::lexer::Lexer;
use crate::parser::{item, PState, Parser};
use crate::ast::Item;
use crate::vm::{VM, Program, Word, Instr};
use crate::ir::{NameTable, Type, Gamma,};

use rustyline::error::ReadlineError;
use rustyline::Editor;
use rustyline::{Completer, Helper, Highlighter, Hinter, Validator};
use rustyline::validate::MatchingBracketValidator;

#[derive(Completer, Helper, Highlighter, Hinter, Validator)]
struct InputValidator {
    #[rustyline(Validator)]
    brackets: MatchingBracketValidator,
}

impl InputValidator {
    fn new() -> Self {
        Self {
            brackets: MatchingBracketValidator::new(),
        }
    }
}

pub fn run_repl() {
    let h = InputValidator::new();
    let mut rl = Editor::new().unwrap();
    rl.set_helper(Some(h));
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
    global: NameTable<Type>,
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
                let t = e.typing(&mut gamma, &self.global)?;
                let _ = e.compile_non_tail(&self.name_table, &mut self.buffer);
                self.buffer.push(Instr::Halt);
                // self.print_buf();
                let addr = self.prog.append(&mut self.buffer);
                let res = self.vm.run(&self.prog, addr);
                self.vm.print(res, &t);
                self.vm.free_mem();
            },
            Item::FunDef(f) => {
                let f = f.de_bruijn_index();
                // println!("{:?}", f);
                let mut gamma = Gamma::new();
                self.global.insert(f.name.clone(), f.ty.clone());
                if let Err(e) = f.type_check(&mut gamma, &self.global) {
                    self.global.remove(&f.name);
                    return Err(e.into());
                }
                let addr = self.prog.size();
                self.name_table.insert(f.name.clone(), addr.into());
                f.compile(&self.name_table, &mut self.buffer);
                self.prog.append(&mut self.buffer);
            },
        }
        Ok(())
    }

    fn print_buf(&self) {
        for i in self.buffer.iter() {
            println!("{}", i);
        }
    }
}

