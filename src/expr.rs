use std::{
    collections::{BTreeMap, HashMap},
    fmt,
};

#[derive(Clone, Debug, PartialEq)]
pub enum IntBinOp {
    Plus,
    Minus,
    Multiply,
    Divide,
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
                let mut labels_str = String::new();
                let mut sep = "";
                for (label, pattern) in labels {
                    labels_str.push_str(sep);
                    labels_str.push_str(&format!("{}: {}", label, pattern));
                    sep = ", ";
                }
                write!(f, "{{{}}}", labels_str)
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

#[derive(Clone, Debug)]
pub enum TypeVar {
    Unbound(Level),
    Link(Type),
    Generic,
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
}
