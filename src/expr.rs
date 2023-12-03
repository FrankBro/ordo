use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt,
};

use itertools::Itertools;

pub const OK_LABEL: &str = "ok";

#[derive(Clone, Debug, PartialEq)]
pub enum IntBinOp {
    Plus,
    Minus,
    Multiply,
    Divide,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

impl fmt::Display for IntBinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op = match self {
            IntBinOp::Plus => "+",
            IntBinOp::Minus => "-",
            IntBinOp::Multiply => "*",
            IntBinOp::Divide => "/",
            IntBinOp::LessThan => "<",
            IntBinOp::LessThanOrEqual => "<=",
            IntBinOp::GreaterThan => ">",
            IntBinOp::GreaterThanOrEqual => ">=",
        };
        write!(f, "{}", op)
    }
}

impl IntBinOp {
    pub fn output_ty(&self) -> Type {
        match self {
            IntBinOp::Plus | IntBinOp::Minus | IntBinOp::Multiply | IntBinOp::Divide => Type::int(),
            IntBinOp::LessThan
            | IntBinOp::LessThanOrEqual
            | IntBinOp::GreaterThan
            | IntBinOp::GreaterThanOrEqual => Type::bool(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Var(String),
    Record(BTreeMap<String, Pattern>),
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pattern::Var(var) => write!(f, "{}", var),
            Pattern::Record(labels) => {
                let labels = labels
                    .iter()
                    .map(|(label, pat)| format!("{}: {}", label, pat))
                    .join(", ");
                write!(f, "{{{}}}", labels)
            }
        }
    }
}

impl Pattern {
    pub fn expr(&self) -> Expr {
        match self {
            Pattern::Var(name) => Expr::Var(name.clone()),
            Pattern::Record(labels) => {
                let labels = labels
                    .iter()
                    .map(|(label, pat)| (label.clone(), pat.expr()))
                    .collect();
                Expr::RecordExtend(labels, Expr::RecordEmpty.into())
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Bool(bool),
    Int(i64),
    IntBinOp(IntBinOp, Box<Expr>, Box<Expr>),
    Negate(Box<Expr>),
    EqualEqual(Box<Expr>, Box<Expr>),
    Var(String),
    Call(Box<Expr>, Vec<Expr>),
    Fun(Vec<Pattern>, Box<Expr>),
    Let(Pattern, Box<Expr>, Box<Expr>),
    RecordSelect(Box<Expr>, String),
    RecordExtend(BTreeMap<String, Expr>, Box<Expr>),
    RecordRestrict(Box<Expr>, String),
    RecordEmpty,
    Variant(String, Box<Expr>),
    Case(
        Box<Expr>,
        Vec<(String, String, Expr)>,
        Option<(String, Box<Expr>)>,
    ),
    If(Box<Expr>, Box<Expr>, Vec<(Expr, Expr)>, Box<Expr>),
    Unwrap(Box<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Bool(b) => write!(f, "{}", b),
            Expr::Int(i) => write!(f, "{}", i),
            Expr::IntBinOp(op, a, b) => write!(f, "{} {} {}", a, op, b),
            Expr::Negate(a) => write!(f, "!{}", a),
            Expr::EqualEqual(a, b) => write!(f, "{} == {}", a, b),
            Expr::Var(v) => write!(f, "{}", v),
            Expr::Call(fun, args) => {
                let args = args.iter().map(|arg| arg.to_string()).join(", ");
                write!(f, "{}({})", fun, args)
            }
            Expr::Fun(params, body) => {
                let params = params.iter().map(|param| param.to_string()).join(", ");
                write!(f, "fun({}) -> {}", params, body)
            }
            Expr::Let(var, val, body) => write!(f, "let {} = {} in {}", var, val, body),
            Expr::RecordSelect(rec, label) => write!(f, "{}.{}", rec, label),
            Expr::RecordExtend(labels, rest) => {
                let labels = labels
                    .iter()
                    .map(|(label, val)| format!("{}: {}", label, val))
                    .join(", ");
                match rest.as_ref() {
                    Expr::RecordEmpty => write!(f, "{{{}}}", labels),
                    _ => write!(f, "{{{} | {}}}", labels, rest),
                }
            }
            Expr::RecordRestrict(record, label) => write!(f, "{}\\{}", record, label),
            Expr::RecordEmpty => write!(f, "{{}}"),
            Expr::Variant(label, value) => write!(f, ":{} {}", label, value),
            Expr::Case(_, _, _) => todo!(),
            Expr::If(_, _, _, _) => todo!(),
            Expr::Unwrap(expr) => write!(f, "{}?", expr),
        }
    }
}

pub type Id = usize;
pub type Level = i64;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Const(String),
    App(Box<Type>, Vec<Type>),
    Arrow(Vec<Type>, Box<Type>),
    Var(Id),
    Record(Box<Type>),
    Variant(Box<Type>),
    RowEmpty,
    RowExtend(BTreeMap<String, Type>, Box<Type>),
}

impl Type {
    pub fn bool() -> Self {
        Self::Const("bool".to_owned())
    }

    pub fn int() -> Self {
        Self::Const("int".to_owned())
    }

    pub fn replace_const_with_vars(self, env: &HashMap<String, Type>) -> Type {
        match self {
            Type::Const(name) => match env.get(&name) {
                Some(ty) => ty.clone(),
                None => Type::Const(name.clone()),
            },
            Type::Var(_) => self,
            Type::App(ty, args) => {
                let ty = ty.replace_const_with_vars(env).into();
                let args = args
                    .into_iter()
                    .map(|arg| arg.replace_const_with_vars(env))
                    .collect();
                Type::App(ty, args)
            }
            Type::Arrow(params, ret) => {
                let params = params
                    .into_iter()
                    .map(|param| param.replace_const_with_vars(env))
                    .collect();
                let ret = ret.replace_const_with_vars(env).into();
                Type::Arrow(params, ret)
            }
            Type::Record(row) => Type::Record(row.replace_const_with_vars(env).into()),
            Type::Variant(row) => Type::Variant(row.replace_const_with_vars(env).into()),
            Type::RowEmpty => Type::RowEmpty,
            Type::RowExtend(labels, rest) => {
                let labels = labels
                    .into_iter()
                    .map(|(label, ty)| (label, ty.replace_const_with_vars(env)))
                    .collect();
                let rest = rest.replace_const_with_vars(env);
                Type::RowExtend(labels, rest.into())
            }
        }
    }
}

pub type Constraints = BTreeSet<String>;

#[derive(Clone, Debug)]
pub enum TypeVar {
    Unbound(Level),
    UnboundRow(Level, Constraints),
    Link(Type),
    Generic,
    GenericRow(Constraints),
}

#[cfg(test)]
pub mod util {
    use super::{Expr, IntBinOp, Pattern};

    pub fn pvar(var: &str) -> Pattern {
        Pattern::Var(var.to_owned())
    }

    pub fn precord(labels: Vec<(&str, Pattern)>) -> Pattern {
        let labels = labels
            .into_iter()
            .map(|(label, pattern)| (label.to_owned(), pattern))
            .collect();
        Pattern::Record(labels)
    }

    pub fn bool(b: bool) -> Expr {
        Expr::Bool(b)
    }

    pub fn int(i: i64) -> Expr {
        Expr::Int(i)
    }

    pub fn var(var: &str) -> Expr {
        Expr::Var(var.to_owned())
    }

    pub fn call(fn_expr: Expr, args: Vec<Expr>) -> Expr {
        Expr::Call(fn_expr.into(), args)
    }

    pub fn fun(args: Vec<Pattern>, body: Expr) -> Expr {
        Expr::Fun(args, body.into())
    }

    pub fn let_(var: Pattern, value: Expr, body: Expr) -> Expr {
        Expr::Let(var, value.into(), body.into())
    }

    pub fn empty() -> Expr {
        Expr::RecordEmpty
    }

    pub fn select(r: Expr, label: &str) -> Expr {
        Expr::RecordSelect(r.into(), label.to_owned())
    }

    pub fn restrict(r: Expr, label: &str) -> Expr {
        Expr::RecordRestrict(r.into(), label.to_owned())
    }

    pub fn record(labels: Vec<(&str, Expr)>, r: Expr) -> Expr {
        let labels = labels
            .into_iter()
            .map(|(label, expr)| (label.to_owned(), expr))
            .collect();
        Expr::RecordExtend(labels, r.into())
    }

    pub fn plus(lhs: Expr, rhs: Expr) -> Expr {
        Expr::IntBinOp(IntBinOp::Plus, lhs.into(), rhs.into())
    }

    pub fn minus(lhs: Expr, rhs: Expr) -> Expr {
        Expr::IntBinOp(IntBinOp::Minus, lhs.into(), rhs.into())
    }

    pub fn multiply(lhs: Expr, rhs: Expr) -> Expr {
        Expr::IntBinOp(IntBinOp::Multiply, lhs.into(), rhs.into())
    }

    pub fn divide(lhs: Expr, rhs: Expr) -> Expr {
        Expr::IntBinOp(IntBinOp::Divide, lhs.into(), rhs.into())
    }

    pub fn negate(expr: Expr) -> Expr {
        Expr::Negate(expr.into())
    }

    pub fn equalequal(lhs: Expr, rhs: Expr) -> Expr {
        Expr::EqualEqual(lhs.into(), rhs.into())
    }

    pub fn gt(lhs: Expr, rhs: Expr) -> Expr {
        Expr::IntBinOp(IntBinOp::GreaterThan, lhs.into(), rhs.into())
    }

    pub fn match_(val: Expr, cases: Vec<(&str, &str, Expr)>, def: Option<(&str, Expr)>) -> Expr {
        let cases = cases
            .into_iter()
            .map(|(variant, var, body)| (variant.to_owned(), var.to_owned(), body))
            .collect();
        let def = def.map(|(var, body)| (var.to_owned(), body.into()));
        Expr::Case(val.into(), cases, def)
    }

    pub fn if_(if_expr: Expr, if_body: Expr, elifs: Vec<(Expr, Expr)>, else_body: Expr) -> Expr {
        Expr::If(if_expr.into(), if_body.into(), elifs, else_body.into())
    }
}
