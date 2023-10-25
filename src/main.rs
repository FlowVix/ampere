#![deny(unused_must_use)]
use std::{
    cell::RefCell,
    fs::File,
    io::{Read, Write},
    path::PathBuf,
    rc::Rc,
};

use ahash::AHashMap;
use colored::Colorize;
use itertools::Itertools;
use lasso::Rodeo;
// use interpreter::{value::Value, Interpreter};
use lexer::Lexer;
use parser::Parser;

use crate::{
    compiler::{Compiler, SourceMap},
    util::slabmap::SlabMap,
    vm::{dummy, Registers, RunInfo, Vm},
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

    let code = src.read().unwrap();
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

    let mut src_map = SourceMap::new();

    match Compiler::new_compile_file(
        &stmts,
        &src,
        &mut interner,
        &mut src_map,
        (0..code.len()).into(),
    ) {
        Ok(c) => {
            src_map.insert(src.clone(), c.build(&src));
        }
        Err(err) => {
            err.into_report().display();
            std::process::exit(1);
        }
    };

    let program = src_map
        .into_iter()
        .map(|(_, b)| b)
        .collect_vec()
        .into_boxed_slice();

    for code in program.iter() {
        code.display();
    }

    let mut vm = Vm {
        memory: SlabMap::new(),
    };
    println!("{}", "------------------------------".dimmed());
    match vm.run_func(
        RunInfo {
            program: &program,
            bytecode_idx: program.len() - 1,
            func_idx: 0,
        },
        dummy,
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
