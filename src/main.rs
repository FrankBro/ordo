use core::make_env;
use std::{
    fmt,
    io::{self, stdout, BufRead, Write},
};

use parser::Parser;

mod core;
mod eval;
mod expr;
mod infer;
mod lexer;
mod parser;

#[derive(Debug)]
enum Error {
    Parser(parser::Error),
    Infer(infer::Error),
    Eval(eval::Error),
}

impl From<parser::Error> for Error {
    fn from(value: parser::Error) -> Self {
        Self::Parser(value)
    }
}

impl From<infer::Error> for Error {
    fn from(value: infer::Error) -> Self {
        Self::Infer(value)
    }
}

impl From<eval::Error> for Error {
    fn from(value: eval::Error) -> Self {
        Self::Eval(value)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Parser(p) => write!(f, "Parser error: {}", p),
            Error::Infer(i) => write!(f, "Infer error: {}", i),
            Error::Eval(e) => write!(f, "Eval error: {}", e),
        }
    }
}

type Result<T, E = Error> = std::result::Result<T, E>;

struct Env {
    infer: infer::Env,
    eval: eval::Env,
}

impl Default for Env {
    fn default() -> Self {
        let infer = make_env();
        let eval = Default::default();
        Self { infer, eval }
    }
}

impl Env {
    fn process(&mut self, source: &str) -> Result<()> {
        let expr = Parser::repl(source)?;
        let ty = self.infer.infer(&expr)?;
        let value = self.eval.eval(&expr)?;
        println!(": {}", self.infer.ty_to_string(&ty).unwrap());
        println!("{}", value);
        Ok(())
    }
}

fn print_ordo() {
    print!("ordo>>> ");
    stdout().flush().unwrap();
}

fn main() {
    let mut env = Env::default();
    let stdin = io::stdin();
    print_ordo();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let source = line.trim_end();
        if let Err(e) = env.process(source) {
            println!("{}", e);
        }
        print_ordo();
    }
}
