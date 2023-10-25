#![deny(unused_must_use)]
use std::{
    cell::RefCell,
    fs::File,
    io::{Read, Write},
    path::PathBuf,
    rc::Rc,
};

use colored::Colorize;
use itertools::Itertools;
use lasso::Rodeo;
// use interpreter::{value::Value, Interpreter};
use lexer::Lexer;
use parser::Parser;

use crate::{
    compiler::Compiler,
    util::slabmap::SlabMap,
    vm::{RunInfo, Vm},
};

mod compiler;
mod error;
// mod interpreter;
mod lexer;
mod parser;
mod source;
mod util;
mod vm;

fn main() {
    print!("\x1B[2J\x1B[1;1H");
    std::io::stdout().flush().unwrap();

    let mut interner = Rodeo::default();

    let src = source::AmpereSource::File(PathBuf::from("test.amp"));
    let src = Rc::new(src);

    let code = src.read();
    let lexer = Lexer::new(&code);
    let mut parser = Parser::new(lexer, &src, &mut interner);

    let stmts = match parser.parse() {
        Ok(v) => {
            // println!("{}", format!("{:#?}", v).bright_green().bold());
            v
        }
        Err(err) => {
            err.into_report().display();
            std::process::exit(1);
        }
    };

    let code = match Compiler::new_compile_file(&stmts, &src, &mut interner, (0..code.len()).into())
    {
        Ok(c) => {
            let b = c.build(&src);
            b.display();
            b
        }
        Err(err) => {
            err.into_report().display();
            std::process::exit(1);
        }
    };

    let program = vec![code].into();

    let mut vm = Vm {
        memory: SlabMap::new(),
    };
    println!("{}", "------------------------------".dimmed());
    match vm.run_func(
        RunInfo {
            program: &program,
            bytecode_idx: 0,
            func_idx: 0,
        },
        |_, _| {},
    ) {
        Ok(_) => {
            // println!(
            //     "{} {}",
            //     "Stack:".bright_blue().bold(),
            //     vm.stack
            //         .iter()
            //         .map(|v| vm.memory[*v].value.to_str(&vm))
            //         .join(&", ".dimmed().to_string())
            // );
        }
        Err(err) => {
            err.into_report().display();
            std::process::exit(1);
        }
    }
}
