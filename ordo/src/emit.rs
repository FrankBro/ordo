use crate::expr::{Expr, ExprIn, ExprTypedAt, IntBinOp, Pattern, PatternTypedAt, Type};

#[derive(Debug)]
pub enum Error {
    NoCode,
}

type Result<T, E = Error> = std::result::Result<T, E>;

pub const LOAD_NAME: &str = ".load";

pub fn emit(expr: ExprTypedAt) -> Result<String> {
    let mut module = Module::default();
    module.emit(expr)?;
    module.function.update_result();
    Ok(module.to_string())
}

#[derive(Clone)]
enum WasmType {
    I32,
    I64,
    // F32,
    // F64,
}

impl From<&Type> for WasmType {
    fn from(value: &Type) -> Self {
        if value == &Type::int() {
            WasmType::I64
        } else if value == &Type::bool() {
            WasmType::I32
        } else {
            todo!("{:?}", value)
        }
    }
}

impl From<Type> for WasmType {
    fn from(value: Type) -> Self {
        WasmType::from(&value)
    }
}

impl std::fmt::Display for WasmType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let t = match self {
            WasmType::I32 => "i32",
            WasmType::I64 => "i64",
            // WasmType::F32 => "f32",
            // WasmType::F64 => "f64",
        };
        write!(f, "{}", t)
    }
}

enum WasmValue {
    // I32(i32),
    I64(i64),
    // F32(f32),
    // F64(f64),
}

impl WasmValue {
    fn ty(&self) -> WasmType {
        match self {
            // WasmValue::I32(_) => WasmType::I32,
            WasmValue::I64(_) => WasmType::I64,
            // WasmValue::F32(_) => WasmType::F32,
            // WasmValue::F64(_) => WasmType::F64,
        }
    }
}

impl From<i64> for WasmValue {
    fn from(value: i64) -> Self {
        Self::I64(value)
    }
}

impl std::fmt::Display for WasmValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // WasmValue::I32(v) => write!(f, "{}", v),
            WasmValue::I64(v) => write!(f, "{}", v),
            // WasmValue::F32(v) => write!(f, "{}", v),
            // WasmValue::F64(v) => write!(f, "{}", v),
        }
    }
}

enum LocalKind {
    Param,
    Local,
}

impl std::fmt::Display for LocalKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            LocalKind::Param => "param",
            LocalKind::Local => "local",
        };
        write!(f, "{}", name)
    }
}

struct WasmLocal {
    kind: LocalKind,
    name: String,
    ty: WasmType,
}

impl WasmLocal {
    fn param(value: PatternTypedAt) -> Self {
        let kind = LocalKind::Param;
        let ty = value.ty().into();
        match *value.expr {
            Pattern::Var(name) => WasmLocal { kind, name, ty },
            _ => todo!(),
        }
    }

    fn local(name: String, ty: WasmType) -> Self {
        let kind = LocalKind::Local;
        Self { kind, name, ty }
    }
}

impl std::fmt::Display for WasmLocal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} ${} {})", self.kind, self.name, self.ty)
    }
}

enum WasmCode {
    LocalGet(String, WasmType),
    Instruction(&'static str, WasmType, WasmType),
    Const(WasmValue),
    Call(String, WasmType),
    LocalSet(String, WasmType),
}

impl std::fmt::Display for WasmCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WasmCode::LocalGet(name, _) => write!(f, "local.get ${}", name),
            WasmCode::Instruction(instruction, in_ty, _) => write!(f, "{}.{}", in_ty, instruction),
            WasmCode::Const(v) => write!(f, "{}.const {}", v.ty(), v),
            WasmCode::Call(name, _) => write!(f, "call ${}", name),
            WasmCode::LocalSet(name, _) => {
                write!(f, "local.set ${}", name)
            }
        }
    }
}

impl WasmCode {
    fn ty(&self) -> WasmType {
        match self {
            WasmCode::LocalGet(_, ty) => ty.clone(),
            WasmCode::Instruction(_, _, out_ty) => out_ty.clone(),
            WasmCode::Const(value) => value.ty(),
            WasmCode::Call(_, ty) => ty.clone(),
            WasmCode::LocalSet(_, ty) => ty.clone(),
        }
    }
}

struct WasmFunction {
    public: bool,
    name: String,
    params: Vec<WasmLocal>,
    locals: Vec<WasmLocal>,
    result: Option<WasmType>,
    code: Vec<WasmCode>,
}

impl WasmFunction {
    fn new(name: String, params: Vec<WasmLocal>) -> Self {
        Self {
            public: !name.starts_with('_'),
            name,
            params,
            locals: Vec::new(),
            result: None,
            code: Vec::new(),
        }
    }

    fn update_result(&mut self) {
        self.result = self.code.last().map(|code| code.ty());
    }
}

impl std::fmt::Display for WasmFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "  (func ${}", self.name)?;
        for param in &self.params {
            write!(f, " {}", param)?;
        }
        if let Some(result) = &self.result {
            write!(f, " (result {})", result)?;
        }
        for local in &self.locals {
            write!(f, " {}", local)?;
        }
        for code in &self.code {
            write!(f, "\n    {}", code)?;
        }
        write!(f, ")")?;
        if self.public {
            write!(f, "\n  (export \"{}\" (func ${}))", self.name, self.name)?;
        }
        Ok(())
    }
}

struct Module {
    functions: Vec<WasmFunction>,
    function: WasmFunction,
}

impl Default for Module {
    fn default() -> Self {
        let functions = Vec::new();
        let function = WasmFunction::new(LOAD_NAME.to_owned(), Vec::new());
        Self {
            functions,
            function,
        }
    }
}

impl Module {
    fn emit(&mut self, expr: ExprTypedAt) -> Result<()> {
        let ty: WasmType = expr.ty().into();
        match *expr.expr {
            Expr::Let(pattern, value, body) => match (*pattern.expr, *value.expr) {
                (Pattern::Var(name), Expr::Fun(params, fun_body)) => {
                    let params = params.into_iter().map(WasmLocal::param).collect();
                    let mut function = WasmFunction::new(name, params);
                    std::mem::swap(&mut self.function, &mut function);
                    self.emit(fun_body)?;
                    std::mem::swap(&mut self.function, &mut function);
                    function.update_result();
                    self.functions.push(function);
                    self.emit(body)
                }
                (Pattern::Var(name), value_expr) => {
                    // TODO: Not ideal to reconstruct this
                    let value = ExprIn {
                        context: value.context,
                        expr: value_expr.into(),
                    };
                    self.emit(value)?;
                    let ty = self
                        .function
                        .code
                        .last()
                        .map(|code| code.ty())
                        .ok_or(Error::NoCode)?;
                    self.function
                        .locals
                        .push(WasmLocal::local(name.clone(), ty.clone()));
                    self.function.code.push(WasmCode::LocalSet(name, ty));
                    self.emit(body)
                }
                _ => todo!(),
            },
            Expr::Int(i) => {
                self.function.code.push(WasmCode::Const(i.into()));
                Ok(())
            }
            Expr::IntBinOp(op, a, b) => {
                let out = op.output_ty().into();
                let i = match op {
                    IntBinOp::Plus => "add",
                    IntBinOp::Minus => "sub",
                    IntBinOp::Multiply => "mul",
                    IntBinOp::Divide => "div_s",
                    IntBinOp::LessThan => "lt_s",
                    IntBinOp::LessThanOrEqual => "le_s",
                    IntBinOp::GreaterThan => "gt_s",
                    IntBinOp::GreaterThanOrEqual => "ge_s",
                };
                self.emit(a)?;
                self.emit(b)?;
                self.function
                    .code
                    .push(WasmCode::Instruction(i, WasmType::I64, out));
                Ok(())
            }
            Expr::Var(name) => {
                self.function.code.push(WasmCode::LocalGet(name, ty));
                Ok(())
            }
            Expr::Call(fun, args) => match *fun.expr {
                Expr::Var(name) => {
                    for arg in args {
                        self.emit(arg)?;
                    }
                    self.function.code.push(WasmCode::Call(name, ty));
                    Ok(())
                }
                _ => todo!(),
            },
            Expr::EqualEqual(a, b) => {
                let in_ty = WasmType::from(&a.context.ty.ty);
                self.emit(a)?;
                self.emit(b)?;
                self.function
                    .code
                    .push(WasmCode::Instruction("eq", in_ty, WasmType::I32));
                Ok(())
            }
            _ => todo!(),
        }
    }
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(module")?;
        for function in &self.functions {
            write!(f, "\n{}", function)?;
        }
        write!(f, "\n{})", self.function)
    }
}
