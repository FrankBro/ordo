use std::fmt;

use crate::{
    core::make_env,
    eval, infer,
    parser::{self, Parser},
};

#[derive(Debug)]
pub enum Error {
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

pub struct Output {
    pub ty: String,
    pub val: String,
}

pub struct Env {
    pub infer: infer::Env,
    pub eval: eval::Env,
}

impl Default for Env {
    fn default() -> Self {
        let infer = make_env();
        let eval = Default::default();
        Self { infer, eval }
    }
}

impl Env {
    pub fn process(&mut self, source: &str) -> Result<Output> {
        let expr = Parser::repl(source)?;
        let typed_expr = self.infer.infer(expr.clone())?;
        let ty = self.infer.ty_to_string(typed_expr.ty()).unwrap();
        let value = self.eval.eval(typed_expr)?;
        let val = value.to_string();
        Ok(Output { ty, val })
    }
}
