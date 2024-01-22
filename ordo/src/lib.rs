pub mod core;
pub mod emit;
pub mod env;
pub mod eval;
pub mod expr;
pub mod infer;
pub mod lexer;
pub mod parser;

#[cfg(test)]
#[path = "tests/ifs.rs"]
mod ifs;

#[cfg(test)]
#[path = "tests/unwraps.rs"]
mod unwraps;

#[cfg(test)]
#[path = "tests/parse_only.rs"]
mod parse_only;

#[cfg(test)]
#[path = "tests/parse_at.rs"]
mod parse_at;

#[cfg(test)]
#[path = "tests/infer_tests.rs"]
mod infer_tests;

#[cfg(test)]
#[path = "tests/typed_exprs.rs"]
mod typed_exprs;

#[cfg(test)]
#[path = "tests/emit_tests.rs"]
mod emit_tests;
