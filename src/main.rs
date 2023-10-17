#![deny(unused_must_use)]
use std::{
    cell::RefCell,
    fs::File,
    io::{Read, Write},
    path::PathBuf,
    rc::Rc,
};

use colored::Colorize;
use interpreter::{value::Value, Interpreter};
use lexer::Lexer;
use parser::Parser;

mod error;
mod interpreter;
mod lexer;
mod parser;
mod source;
mod util;

fn main() {
    print!("\x1B[2J\x1B[1;1H");
    std::io::stdout().flush().unwrap();

    let src = source::AmpereSource::File(PathBuf::from("test.amp"));
    let src = Rc::new(src);

    let code = src.read();
    let lexer = Lexer::new(&code);
    let mut parser = Parser::new(lexer, &src);

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

    let result = match Interpreter::new_run_file(&stmts, &src) {
        Ok(v) => v,
        Err(err) => {
            err.into_report().display();
            std::process::exit(1);
        }
    };
    // .map(|v| v.value)
    // .unwrap_or(Value::unit());

    // println!("{}", result.display().bright_green());
}
