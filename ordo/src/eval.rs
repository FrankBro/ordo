use core::fmt;
use std::collections::BTreeMap;

use itertools::Itertools;

use crate::expr::{Expr, ExprAt, IntBinOp, Pattern, PatternAt, OK_LABEL};

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
    UnwrapNotVariant(Value),
    PatternRecordRestNotEmpty(ExprAt),
    InvalidPattern(ExprAt),
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
            Error::UnwrapNotVariant(val) => write!(f, "unwrap on a non-variant: {}", val),
            Error::PatternRecordRestNotEmpty(expr) => {
                write!(f, "record pattern extended something: {}", expr)
            }
            Error::InvalidPattern(expr) => write!(f, "invalid pattern: {}", expr),
        }
    }
}
type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    env: Env,
    params: Vec<PatternAt>,
    body: ExprAt,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params = self.params.iter().map(|param| param.to_string()).join(", ");
        write!(f, "fun({})", params)
    }
}

impl Function {
    fn apply(&mut self, args: Vec<Value>) -> Result<Wrap> {
        if self.params.len() != args.len() {
            return Err(Error::UnexpectedNumberOfArguments);
        }
        for (i, arg) in args.into_iter().enumerate() {
            let param = &self.params[i];
            self.env.eval_pattern(param, arg)?;
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
    pub fn record(labels: Vec<(&str, Value)>) -> Self {
        let labels = labels
            .into_iter()
            .map(|(label, val)| (label.to_owned(), val))
            .collect();
        Value::Record(labels)
    }

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

enum Wrap {
    Value(Value),
    Wrap(String, Box<Value>),
}

impl Wrap {
    fn value(self) -> Value {
        match self {
            Wrap::Value(val) => val,
            Wrap::Wrap(name, val) => Value::Variant(name, val),
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Env {
    pub vars: BTreeMap<String, Value>,
}

impl Env {
    pub fn eval(&mut self, expr: &ExprAt) -> Result<Value> {
        let wrap = self.eval_inner(expr)?;
        Ok(wrap.value())
    }

    fn eval_pattern(&mut self, pattern: &PatternAt, value: Value) -> Result<()> {
        match pattern.expr.as_ref() {
            Pattern::Var(var) => {
                self.vars.insert(var.clone(), value);
            }
            Pattern::RecordExtend(labels, rest) => {
                match rest.expr.as_ref() {
                    Expr::RecordEmpty => (),
                    _ => return Err(Error::PatternRecordRestNotEmpty(rest.clone())),
                }
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
            _ => return Err(Error::InvalidPattern(pattern.clone())),
        }
        Ok(())
    }

    fn eval_inner(&mut self, expr: &ExprAt) -> Result<Wrap> {
        match expr.expr.as_ref() {
            Expr::Bool(b) => Ok(Wrap::Value(Value::Bool(*b))),
            Expr::Int(i) => Ok(Wrap::Value(Value::Int(*i))),
            Expr::IntBinOp(op, lhs, rhs) => {
                let lhs = match self.eval_inner(lhs)? {
                    Wrap::Value(value) => value.as_int()?,
                    Wrap::Wrap(name, val) => return Ok(Wrap::Value(Value::Variant(name, val))),
                };
                let rhs = match self.eval_inner(rhs)? {
                    Wrap::Value(value) => value.as_int()?,
                    Wrap::Wrap(name, val) => return Ok(Wrap::Value(Value::Variant(name, val))),
                };
                let val = match op {
                    IntBinOp::Plus => Value::Int(lhs + rhs),
                    IntBinOp::Minus => Value::Int(lhs - rhs),
                    IntBinOp::Multiply => Value::Int(lhs * rhs),
                    IntBinOp::Divide => Value::Int(lhs / rhs),
                    IntBinOp::LessThan => Value::Bool(lhs < rhs),
                    IntBinOp::LessThanOrEqual => Value::Bool(lhs <= rhs),
                    IntBinOp::GreaterThan => Value::Bool(lhs > rhs),
                    IntBinOp::GreaterThanOrEqual => Value::Bool(lhs >= rhs),
                };
                Ok(Wrap::Value(val))
            }
            Expr::Negate(expr) => {
                let b = match self.eval_inner(expr)? {
                    Wrap::Value(value) => value.as_bool()?,
                    Wrap::Wrap(name, val) => return Ok(Wrap::Value(Value::Variant(name, val))),
                };
                Ok(Wrap::Value(Value::Bool(!b)))
            }
            Expr::EqualEqual(lhs, rhs) => {
                let lhs = match self.eval_inner(lhs)? {
                    Wrap::Value(value) => value,
                    Wrap::Wrap(name, val) => return Ok(Wrap::Value(Value::Variant(name, val))),
                };
                let rhs = match self.eval_inner(rhs)? {
                    Wrap::Value(value) => value,
                    Wrap::Wrap(name, val) => return Ok(Wrap::Value(Value::Variant(name, val))),
                };
                Ok(Wrap::Value(Value::Bool(lhs == rhs)))
            }
            Expr::Var(s) => {
                let value = self
                    .vars
                    .get(s)
                    .ok_or_else(|| Error::VarNotFound(s.clone()))?;
                Ok(Wrap::Value(value.clone()))
            }
            Expr::Call(fun, args) => {
                let mut fun = match self.eval_inner(fun)? {
                    Wrap::Value(value) => value.as_function()?,
                    Wrap::Wrap(name, val) => return Ok(Wrap::Value(Value::Variant(name, val))),
                };
                let mut values = Vec::with_capacity(args.len());
                for arg in args {
                    let value = match self.eval_inner(arg)? {
                        Wrap::Value(value) => value,
                        Wrap::Wrap(name, val) => return Ok(Wrap::Value(Value::Variant(name, val))),
                    };
                    values.push(value);
                }
                fun.apply(values)
            }
            Expr::Fun(params, body) => {
                let env = self.clone();
                let params = params.clone();
                let body = body.clone();
                let fun = Function { env, params, body };
                Ok(Wrap::Value(Value::Function(fun)))
            }
            Expr::Let(var, val, body) => {
                let val = match self.eval_inner(val)? {
                    Wrap::Value(value) => value,
                    Wrap::Wrap(name, val) => return Ok(Wrap::Value(Value::Variant(name, val))),
                };
                self.eval_pattern(var, val)?;
                self.eval_inner(body)
            }
            Expr::RecordSelect(record, label) => {
                let record = match self.eval_inner(record)? {
                    Wrap::Value(value) => value,
                    Wrap::Wrap(name, val) => return Ok(Wrap::Value(Value::Variant(name, val))),
                };
                let record = record.as_record()?;
                let val = record
                    .get(label)
                    .ok_or_else(|| Error::LabelNotFound(label.clone()))?;
                Ok(Wrap::Value(val.clone()))
            }
            Expr::RecordExtend(labels, record) => {
                let record = match self.eval_inner(record)? {
                    Wrap::Value(value) => value,
                    Wrap::Wrap(name, val) => return Ok(Wrap::Value(Value::Variant(name, val))),
                };
                let record = record.as_record()?;
                let mut record = record.clone();
                for (label, expr) in labels {
                    let value = match self.eval_inner(expr)? {
                        Wrap::Value(value) => value,
                        Wrap::Wrap(name, val) => return Ok(Wrap::Value(Value::Variant(name, val))),
                    };
                    record.insert(label.clone(), value);
                }
                Ok(Wrap::Value(Value::Record(record)))
            }
            Expr::RecordRestrict(record, label) => {
                let record = match self.eval_inner(record)? {
                    Wrap::Value(value) => value,
                    Wrap::Wrap(name, val) => return Ok(Wrap::Value(Value::Variant(name, val))),
                };
                let record = record.as_record()?;
                let mut record = record.clone();
                record.remove(label);
                Ok(Wrap::Value(Value::Record(record)))
            }
            Expr::RecordEmpty => Ok(Wrap::Value(Value::Record(BTreeMap::new()))),
            Expr::Variant(label, expr) => {
                let value = match self.eval_inner(expr)? {
                    Wrap::Value(value) => value,
                    Wrap::Wrap(name, val) => return Ok(Wrap::Value(Value::Variant(name, val))),
                };
                Ok(Wrap::Value(Value::Variant(label.clone(), value.into())))
            }
            Expr::Case(value, cases, def) => {
                let value = match self.eval_inner(value)? {
                    Wrap::Value(value) => value,
                    Wrap::Wrap(name, val) => return Ok(Wrap::Value(Value::Variant(name, val))),
                };
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
            Expr::If(if_expr, if_body, elifs, else_body) => {
                let b = match self.eval_inner(if_expr)? {
                    Wrap::Value(value) => value.as_bool()?,
                    Wrap::Wrap(name, val) => return Ok(Wrap::Value(Value::Variant(name, val))),
                };
                if b {
                    return self.eval_inner(if_body);
                }
                for (elif_expr, elif_body) in elifs {
                    let b = match self.eval_inner(elif_expr)? {
                        Wrap::Value(value) => value.as_bool()?,
                        Wrap::Wrap(name, val) => return Ok(Wrap::Value(Value::Variant(name, val))),
                    };
                    if b {
                        return self.eval_inner(elif_body);
                    }
                }
                self.eval_inner(else_body)
            }
            Expr::Unwrap(expr) => {
                let val = match self.eval_inner(expr)? {
                    Wrap::Value(value) => value,
                    Wrap::Wrap(name, val) => return Ok(Wrap::Value(Value::Variant(name, val))),
                };
                match val {
                    Value::Variant(name, val) if name == OK_LABEL => Ok(Wrap::Value(*val)),
                    Value::Variant(name, val) => Ok(Wrap::Wrap(name, val)),
                    val => Err(Error::UnwrapNotVariant(val)),
                }
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
            ("match :a 3 { :b b -> b , :a a -> a }", Value::Int(3)),
            ("let a = 1 in let r = {a} in r.a", Value::Int(1)),
            (
                "let f({x,y}) = x + y in let x = 1 in let y = 2 in f({x,y})",
                Value::Int(3),
            ),
            ("2 > 1", Value::Bool(true)),
            ("2 > 3", Value::Bool(false)),
        ];
        for (expr_str, expected) in cases {
            let expr = Parser::expr(expr_str).unwrap();
            let actual = Env::default().eval(&expr).unwrap();
            assert_eq!(expected, actual);
        }
    }
}
