#![deny(unused_must_use)]
use std::{fs::File, io::Read, path::PathBuf, rc::Rc};

use colored::Colorize;
use error::ErrorDisplay;
use lexer::Lexer;
use parser::Parser;

mod error;
mod lexer;
mod parser;
mod source;

fn main() {
    let src = source::AmpereSource::File(PathBuf::from("test.amp"));

    let code = src.read();
    let lexer = Lexer::new(&code);
    let mut parser = Parser::new(lexer, Rc::new(src));

    match parser.parse_expr() {
        Ok(v) => println!("{}", format!("{:#?}", v).bright_green().bold()),
        Err(err) => {
            // println!("{}", format!("{:#?}", err).bright_red().bold())
            let display: ErrorDisplay = err.into();
            display.display();
        }
    }
}
