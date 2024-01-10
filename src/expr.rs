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

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Location {
    pub start: Position,
    pub end: Position,
}

#[derive(Clone, Debug, PartialEq)]
pub struct At<T> {
    pub start: Position,
    pub end: Position,
    pub value: T,
}

impl<T> At<T> {
    pub fn map<U, F>(self, f: F) -> At<U>
    where
        F: FnOnce(T) -> U,
    {
        let start = self.start;
        let end = self.end;
        let value = f(self.value);
        At { start, end, value }
    }

    pub fn span_with<N, V>(self, next: At<N>, value: V) -> At<V> {
        At {
            start: self.start,
            end: next.end,
            value,
        }
    }
}

pub type NoContext = ();

#[derive(Clone, Debug, PartialEq)]
pub struct PositionContext {
    pub start: Position,
    pub end: Position,
}

impl<T> From<At<T>> for PositionContext {
    fn from(value: At<T>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeContext {
    pub ty: Type,
}

impl From<Type> for TypeContext {
    fn from(ty: Type) -> Self {
        Self { ty }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprIn<Context> {
    pub context: Context,
    pub expr: Box<Expr<Context>>,
}

impl<Context> fmt::Display for ExprIn<Context> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expr)
    }
}

impl From<Expr<NoContext>> for ExprOnly {
    fn from(expr: Expr<NoContext>) -> Self {
        ExprIn {
            context: (),
            expr: expr.into(),
        }
    }
}

impl From<At<Expr<PositionContext>>> for ExprAt {
    fn from(value: At<Expr<PositionContext>>) -> Self {
        ExprIn {
            context: PositionContext {
                start: value.start,
                end: value.end,
            },
            expr: value.value.into(),
        }
    }
}

impl ExprIn<PositionContext> {
    pub fn strip_context(self) -> ExprIn<NoContext> {
        ExprIn {
            context: (),
            expr: self.expr.strip_context().into(),
        }
    }
}

impl ExprIn<PositionTypeContext> {
    pub fn strip_position(self) -> ExprIn<TypeContext> {
        ExprIn {
            context: self.context.ty,
            expr: self.expr.strip_position().into(),
        }
    }

    pub fn ty(&self) -> &Type {
        &self.context.ty.ty
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PositionTypeContext {
    pub position: PositionContext,
    pub ty: TypeContext,
}

pub type ExprOnly = ExprIn<NoContext>;
pub type ExprAt = ExprIn<PositionContext>;
pub type ExprTyped = ExprIn<TypeContext>;
pub type ExprTypedAt = ExprIn<PositionTypeContext>;

pub type Pattern<Context> = Expr<Context>;
pub type PatternIn<Context> = ExprIn<Context>;
pub type PatternOnly = ExprOnly;
pub type PatternAt = ExprAt;
pub type PatternTypedAt = ExprTypedAt;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<Context> {
    Bool(bool),
    Int(i64),
    IntBinOp(IntBinOp, ExprIn<Context>, ExprIn<Context>),
    Negate(ExprIn<Context>),
    EqualEqual(ExprIn<Context>, ExprIn<Context>),
    Var(String),
    Call(ExprIn<Context>, Vec<ExprIn<Context>>),
    Fun(Vec<PatternIn<Context>>, ExprIn<Context>),
    Let(PatternIn<Context>, ExprIn<Context>, ExprIn<Context>),
    RecordSelect(ExprIn<Context>, String),
    RecordExtend(BTreeMap<String, ExprIn<Context>>, ExprIn<Context>),
    RecordRestrict(ExprIn<Context>, String),
    RecordEmpty,
    Variant(String, ExprIn<Context>),
    Case(
        ExprIn<Context>,
        Vec<(String, String, ExprIn<Context>)>,
        Option<(String, ExprIn<Context>)>,
    ),
    If(
        ExprIn<Context>,
        ExprIn<Context>,
        Vec<(ExprIn<Context>, ExprIn<Context>)>,
        ExprIn<Context>,
    ),
    Unwrap(ExprIn<Context>),
}

impl<Context> fmt::Display for Expr<Context> {
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
            Expr::Let(var, val, body) => {
                write!(f, "let {} = {} in {}", var, val, body)
            }
            Expr::RecordSelect(rec, label) => write!(f, "{}.{}", rec, label),
            Expr::RecordExtend(labels, rest) => {
                let labels = labels
                    .iter()
                    .map(|(label, val)| format!("{}: {}", label, val))
                    .join(", ");
                match rest.expr.as_ref() {
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

impl Expr<PositionContext> {
    fn strip_context(self) -> Expr<NoContext> {
        fn fix(e: ExprAt) -> ExprOnly {
            e.strip_context()
        }
        match self {
            Expr::Bool(b) => Expr::Bool(b),
            Expr::Int(i) => Expr::Int(i),
            Expr::IntBinOp(op, a, b) => Expr::IntBinOp(op, fix(a), fix(b)),
            Expr::Negate(e) => Expr::Negate(fix(e)),
            Expr::EqualEqual(a, b) => Expr::EqualEqual(fix(a), fix(b)),
            Expr::Var(s) => Expr::Var(s),
            Expr::Call(fun, args) => {
                let args = args.into_iter().map(ExprAt::strip_context).collect();
                Expr::Call(fix(fun), args)
            }
            Expr::Fun(params, body) => {
                let params = params.into_iter().map(ExprAt::strip_context).collect();
                Expr::Fun(params, fix(body))
            }
            Expr::Let(p, v, b) => Expr::Let(fix(p), fix(v), fix(b)),
            Expr::RecordSelect(r, l) => Expr::RecordSelect(fix(r), l),
            Expr::RecordExtend(ls, r) => {
                let ls = ls
                    .into_iter()
                    .map(|(l, e)| (l, e.strip_context()))
                    .collect();
                Expr::RecordExtend(ls, fix(r))
            }
            Expr::RecordRestrict(r, l) => Expr::RecordRestrict(fix(r), l),
            Expr::RecordEmpty => Expr::RecordEmpty,
            Expr::Variant(l, e) => Expr::Variant(l, fix(e)),
            Expr::Case(e, cs, d) => {
                let cs = cs
                    .into_iter()
                    .map(|(l, v, b)| (l, v, b.strip_context()))
                    .collect();
                let d = d.map(|(v, b)| (v, fix(b)));
                Expr::Case(fix(e), cs, d)
            }
            Expr::If(i, ib, ies, eb) => {
                let ies = ies
                    .into_iter()
                    .map(|(ie, ieb)| (ie.strip_context(), ieb.strip_context()))
                    .collect();
                Expr::If(fix(i), fix(ib), ies, fix(eb))
            }
            Expr::Unwrap(e) => Expr::Unwrap(fix(e)),
        }
    }
}

impl Expr<PositionTypeContext> {
    pub fn with(self, position: PositionContext, ty: Type) -> ExprTypedAt {
        let ty = TypeContext { ty };
        ExprIn {
            context: PositionTypeContext { position, ty },
            expr: self.into(),
        }
    }

    pub fn strip_position(self) -> Expr<TypeContext> {
        fn fix(e: ExprTypedAt) -> ExprTyped {
            e.strip_position()
        }
        match self {
            Expr::Bool(b) => Expr::Bool(b),
            Expr::Int(i) => Expr::Int(i),
            Expr::IntBinOp(op, a, b) => Expr::IntBinOp(op, fix(a), fix(b)),
            Expr::Negate(e) => Expr::Negate(fix(e)),
            Expr::EqualEqual(a, b) => Expr::EqualEqual(fix(a), fix(b)),
            Expr::Var(s) => Expr::Var(s),
            Expr::Call(fun, args) => {
                let args = args.into_iter().map(ExprTypedAt::strip_position).collect();
                Expr::Call(fix(fun), args)
            }
            Expr::Fun(params, body) => {
                let params = params
                    .into_iter()
                    .map(ExprTypedAt::strip_position)
                    .collect();
                Expr::Fun(params, fix(body))
            }
            Expr::Let(p, v, b) => Expr::Let(fix(p), fix(v), fix(b)),
            Expr::RecordSelect(r, l) => Expr::RecordSelect(fix(r), l),
            Expr::RecordExtend(ls, r) => {
                let ls = ls
                    .into_iter()
                    .map(|(l, e)| (l, e.strip_position()))
                    .collect();
                Expr::RecordExtend(ls, fix(r))
            }
            Expr::RecordRestrict(r, l) => Expr::RecordRestrict(fix(r), l),
            Expr::RecordEmpty => Expr::RecordEmpty,
            Expr::Variant(l, e) => Expr::Variant(l, fix(e)),
            Expr::Case(e, cs, d) => {
                let cs = cs
                    .into_iter()
                    .map(|(l, v, b)| (l, v, b.strip_position()))
                    .collect();
                let d = d.map(|(v, b)| (v, fix(b)));
                Expr::Case(fix(e), cs, d)
            }
            Expr::If(i, ib, ies, eb) => {
                let ies = ies
                    .into_iter()
                    .map(|(ie, ieb)| (ie.strip_position(), ieb.strip_position()))
                    .collect();
                Expr::If(fix(i), fix(ib), ies, fix(eb))
            }
            Expr::Unwrap(e) => Expr::Unwrap(fix(e)),
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
