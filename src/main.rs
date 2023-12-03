use core::make_env;
use std::{
    fmt,
    io::{self, stdout, BufRead, Write},
};

use parser::Parser;

pub mod core;
pub mod eval;
pub mod expr;
pub mod infer;
pub mod lexer;
pub mod parser;

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
            Error::Parser(p) => write!(f, "parser: {}", p),
            Error::Infer(i) => write!(f, "infer: {}", i),
            Error::Eval(e) => write!(f, "eval: {}", e),
        }
    }
}

type Result<T, E = Error> = std::result::Result<T, E>;

struct Output {
    ty: String,
    val: String,
}

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
    fn process(&mut self, source: &str) -> Result<Output> {
        let expr = Parser::repl(source)?;
        let ty = self.infer.infer(&expr)?;
        let value = self.eval.eval(&expr)?;
        let ty = self.infer.ty_to_string(&ty).unwrap();
        let val = value.to_string();
        Ok(Output { ty, val })
    }
}

const ORDO: &str = "ordo>>> ";
const ERROR: &str = "error: ";

fn print_ordo() {
    print!("{}", ORDO);
    stdout().flush().unwrap();
}

fn main() {
    let mut env = Env::default();
    let stdin = io::stdin();
    print_ordo();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let source = line.trim_end();
        match env.process(source) {
            Ok(output) => {
                println!("{}", output.ty);
                println!("{}", output.val);
            }
            Err(e) => {
                println!("{}{}", ERROR, e);
            }
        }
        print_ordo();
    }
}

#[test]
fn readme_test() {
    let file = std::fs::read_to_string("README.md").unwrap();
    let lines: Vec<&str> = file.lines().collect();
    let mut i = 0;

    let mut env = Env::default();

    while i < lines.len() {
        if lines[i].starts_with(ORDO) {
            let source = lines[i];
            let source = source.strip_prefix(ORDO).unwrap();
            let expected = lines[i + 1];
            let actual = env.process(source);
            match actual {
                Ok(actual) => {
                    if expected.starts_with(ERROR) {
                        panic!(
                            "input '{}' succeeded but expected an error: {}",
                            source, expected
                        );
                    } else {
                        let expected_ty = expected;
                        let expected_val = lines[i + 2];
                        assert_eq!(expected_ty, actual.ty, "for {}", source);
                        assert_eq!(expected_val, actual.val, "for {}", source);
                        i += 3;
                    }
                }
                Err(actual) => {
                    if expected.starts_with(ERROR) {
                        let actual = format!("{}{}", ERROR, actual);
                        assert_eq!(expected, &actual);
                        i += 2;
                    } else {
                        panic!(
                            "input '{}' error '{}' but expected a success: {}",
                            source, actual, expected
                        );
                    }
                }
            }
        } else {
            i += 1;
        }
    }
}

#[cfg(test)]
#[path = "tests/ifs.rs"]
mod ifs;

#[cfg(test)]
#[path = "tests/unwraps.rs"]
mod unwraps;
