use core::fmt;
use std::collections::{BTreeMap, HashMap};

use itertools::Itertools;

use crate::expr::{Expr, IntBinOp, Pattern};

#[derive(Debug)]
pub enum Error {
    NotBool(Value),
    NotInt(Value),
    NotFunction(Value),
    NotRecord(Value),
    NotVariant(Value),
    VarNotFound(String),
    UnexpectedNumberOfArguments,
    LabelNotFound(String),
    NoCase,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::NotBool(v) => write!(f, "expected bool, got {}", v),
            Error::NotInt(v) => write!(f, "expected int, got {}", v),
            Error::NotFunction(v) => write!(f, "expected function, got {}", v),
            Error::NotRecord(v) => write!(f, "expected record, got {}", v),
            Error::NotVariant(v) => write!(f, "expected variant, got {}", v),
            Error::VarNotFound(var) => write!(f, "variable not found: {}", var),
            Error::UnexpectedNumberOfArguments => write!(f, "unexpected number of arguments"),
            Error::LabelNotFound(label) => write!(f, "label not found: {}", label),
            Error::NoCase => write!(f, "no case"),
        }
    }
}
type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    env: Env,
    params: Vec<Pattern>,
    body: Expr,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params = self.params.iter().map(|param| param.to_string()).join(", ");
        write!(f, "fun({})", params)
    }
}

impl Function {
    fn apply(&mut self, args: &Vec<Expr>) -> Result<Value> {
        if self.params.len() != args.len() {
            return Err(Error::UnexpectedNumberOfArguments);
        }
        for (i, arg) in args.iter().enumerate() {
            let param = &self.params[i];
            let val = self.env.eval_inner(arg)?;
            self.env.eval_pattern(param, val)?;
        }
        self.env.eval_inner(&self.body)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    Int(i64),
    Function(Function),
    Record(BTreeMap<String, Value>),
    Variant(String, Box<Value>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(i) => write!(f, "{}", i),
            Value::Function(fun) => write!(f, "{}", fun),
            Value::Record(labels) => {
                let labels = labels
                    .iter()
                    .map(|(label, val)| format!("{}: {}", label, val))
                    .join(", ");
                write!(f, "{{{}}}", labels)
            }
            Value::Variant(label, value) => {
                write!(f, ":{} {}", label, value)
            }
        }
    }
}

impl Value {
    fn as_bool(&self) -> Result<bool> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => Err(Error::NotBool(self.clone())),
        }
    }

    fn as_int(&self) -> Result<i64> {
        match self {
            Value::Int(i) => Ok(*i),
            _ => Err(Error::NotInt(self.clone())),
        }
    }

    fn as_function(&self) -> Result<Function> {
        match self {
            Value::Function(fun) => Ok(fun.clone()),
            _ => Err(Error::NotFunction(self.clone())),
        }
    }

    fn as_record(&self) -> Result<&BTreeMap<String, Value>> {
        match self {
            Value::Record(record) => Ok(record),
            _ => Err(Error::NotRecord(self.clone())),
        }
    }

    fn as_variant(&self) -> Result<(&String, &Value)> {
        match self {
            Value::Variant(label, value) => Ok((label, value)),
            _ => Err(Error::NotVariant(self.clone())),
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Env {
    vars: HashMap<String, Value>,
}

impl Env {
    pub fn eval(&mut self, expr: &Expr) -> Result<Value> {
        self.eval_inner(expr)
    }

    fn eval_pattern(&mut self, pattern: &Pattern, value: Value) -> Result<()> {
        match pattern {
            Pattern::Var(var) => {
                self.vars.insert(var.clone(), value);
            }
            Pattern::Record(labels) => {
                let labels_value = match value {
                    Value::Record(labels) => labels,
                    _ => return Err(Error::NotRecord(value)),
                };
                for (label, label_pattern) in labels {
                    match labels_value.get(label) {
                        None => return Err(Error::LabelNotFound(label.clone())),
                        Some(label_value) => {
                            self.eval_pattern(label_pattern, label_value.clone())?
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn eval_inner(&mut self, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Bool(b) => Ok(Value::Bool(*b)),
            Expr::Int(i) => Ok(Value::Int(*i)),
            Expr::IntBinOp(op, lhs, rhs) => {
                let lhs = self.eval_inner(lhs)?;
                let lhs = lhs.as_int()?;
                let rhs = self.eval_inner(rhs)?;
                let rhs = rhs.as_int()?;
                let i = match op {
                    IntBinOp::Plus => lhs + rhs,
                    IntBinOp::Minus => lhs - rhs,
                    IntBinOp::Multiply => lhs * rhs,
                    IntBinOp::Divide => lhs / rhs,
                };
                Ok(Value::Int(i))
            }
            Expr::Negate(expr) => {
                let v = self.eval_inner(expr)?;
                let b = v.as_bool()?;
                Ok(Value::Bool(!b))
            }
            Expr::EqualEqual(lhs, rhs) => {
                let lhs = self.eval_inner(lhs)?;
                let rhs = self.eval_inner(rhs)?;
                Ok(Value::Bool(lhs == rhs))
            }
            Expr::Var(s) => {
                let value = self
                    .vars
                    .get(s)
                    .ok_or_else(|| Error::VarNotFound(s.clone()))?;
                Ok(value.clone())
            }
            Expr::Call(fun, args) => {
                let fun = self.eval_inner(fun)?;
                let mut fun = fun.as_function()?;
                fun.apply(args)
            }
            Expr::Fun(params, body) => {
                let env = self.clone();
                let params = params.clone();
                let body = *body.clone();
                let fun = Function { env, params, body };
                Ok(Value::Function(fun))
            }
            Expr::Let(var, val, body) => {
                let val = self.eval_inner(val)?;
                self.eval_pattern(var, val)?;
                self.eval_inner(body)
            }
            Expr::RecordSelect(record, label) => {
                let record = self.eval_inner(record)?;
                let record = record.as_record()?;
                let val = record
                    .get(label)
                    .ok_or_else(|| Error::LabelNotFound(label.clone()))?;
                Ok(val.clone())
            }
            Expr::RecordExtend(labels, record) => {
                let record = self.eval_inner(record)?;
                let record = record.as_record()?;
                let mut record = record.clone();
                for (label, expr) in labels {
                    let value = self.eval_inner(expr)?;
                    record.insert(label.clone(), value);
                }
                Ok(Value::Record(record))
            }
            Expr::RecordRestrict(record, label) => {
                let record = self.eval_inner(record)?;
                let record = record.as_record()?;
                let mut record = record.clone();
                record.remove(label);
                Ok(Value::Record(record))
            }
            Expr::RecordEmpty => Ok(Value::Record(BTreeMap::new())),
            Expr::Variant(label, expr) => {
                let value = self.eval_inner(expr)?;
                Ok(Value::Variant(label.clone(), value.into()))
            }
            Expr::Case(value, cases, def) => {
                let value = self.eval_inner(value)?;
                let (label, value) = value.as_variant()?;
                for (case_label, case_var, case_body) in cases {
                    if label == case_label {
                        let mut env = self.clone();
                        env.vars.insert(case_var.clone(), value.clone());
                        return env.eval_inner(case_body);
                    }
                }
                if let Some((var, body)) = def {
                    let mut env = self.clone();
                    env.vars.insert(
                        var.clone(),
                        Value::Variant(label.clone(), value.clone().into()),
                    );
                    return env.eval_inner(body);
                }
                Err(Error::NoCase)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Parser;

    use super::{Env, Value};

    #[test]
    fn tests() {
        let cases: Vec<(&str, Value)> = vec![
            ("1 + 2", Value::Int(3)),
            ("let a = 1 in a + 2", Value::Int(3)),
            ("{a = 2}.a", Value::Int(2)),
            (":a 3", Value::Variant("a".to_owned(), Value::Int(3).into())),
            ("match :a 3 { :b b -> b | :a a -> a }", Value::Int(3)),
            ("let a = 1 in let r = {a} in r.a", Value::Int(1)),
        ];
        for (expr_str, expected) in cases {
            let expr = Parser::expr(expr_str).unwrap();
            let actual = Env::default().eval(&expr).unwrap();
            assert_eq!(expected, actual);
        }
    }
}
