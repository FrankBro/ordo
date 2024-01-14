use crate::expr::{Expr, ExprTypedAt, IntBinOp, Pattern, PatternTypedAt, Type};

#[derive(Debug)]
pub enum Error {}

type Result<T, E = Error> = std::result::Result<T, E>;

pub fn emit(expr: ExprTypedAt) -> Result<String> {
    let mut module = Module::default();
    module.emit(expr)?;
    Ok(module.to_string())
}

#[derive(Clone)]
enum WasmType {
    // I32,
    I64,
    // F32,
    // F64,
}

impl From<&Type> for WasmType {
    fn from(value: &Type) -> Self {
        if value == &Type::int() {
            WasmType::I64
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
            // WasmType::I32 => "i32",
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

struct WasmParam {
    name: String,
    ty: WasmType,
}

impl From<PatternTypedAt> for WasmParam {
    fn from(value: PatternTypedAt) -> Self {
        let ty = value.ty().into();
        match *value.expr {
            Pattern::Var(name) => WasmParam { name, ty },
            _ => todo!(),
        }
    }
}

impl std::fmt::Display for WasmParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(param ${} {})", self.name, self.ty)
    }
}

enum WasmCode {
    LocalGet(String, WasmType),
    Add(WasmType),
    Const(WasmValue),
    Call(String, WasmType),
}

impl std::fmt::Display for WasmCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WasmCode::LocalGet(name, _) => write!(f, "local.get ${}", name),
            WasmCode::Add(ty) => write!(f, "{}.add", ty),
            WasmCode::Const(v) => write!(f, "{}.const {}", v.ty(), v),
            WasmCode::Call(name, _) => write!(f, "call ${}", name),
        }
    }
}

impl WasmCode {
    fn ty(&self) -> WasmType {
        match self {
            WasmCode::LocalGet(_, ty) => ty.clone(),
            WasmCode::Add(ty) => ty.clone(),
            WasmCode::Const(value) => value.ty(),
            WasmCode::Call(_, ty) => ty.clone(),
        }
    }
}

struct WasmFunction {
    public: bool,
    name: String,
    params: Vec<WasmParam>,
    result: Option<WasmType>,
    code: Vec<WasmCode>,
}

impl std::fmt::Display for WasmFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "  (func ${}", self.name)?;
        for param in &self.params {
            write!(f, "{} ", param)?;
        }
        if let Some(result) = &self.result {
            write!(f, "(result {})", result)?;
        }
        for code in &self.code {
            write!(f, "\n    {}", code)?;
        }
        write!(f, ")")?;
        if self.public {
            write!(f, "  (export \"{}\" (func ${})", self.name, self.name)?;
        }
        Ok(())
    }
}

#[derive(Default)]
struct Module {
    functions: Vec<WasmFunction>,
    code: Vec<WasmCode>,
}

impl Module {
    fn emit(&mut self, expr: ExprTypedAt) -> Result<()> {
        let ty: WasmType = expr.ty().into();
        match *expr.expr {
            Expr::Let(pattern, body, rest) => match (*pattern.expr, *body.expr) {
                (Pattern::Var(name), Expr::Fun(params, body)) => {
                    let params = params.into_iter().map(WasmParam::from).collect();
                    let mut code = Vec::new();
                    std::mem::swap(&mut self.code, &mut code);
                    self.emit(body)?;
                    std::mem::swap(&mut self.code, &mut code);
                    let result = code.last().map(WasmCode::ty);
                    let function = WasmFunction {
                        public: name.starts_with('_'),
                        name,
                        params,
                        result,
                        code,
                    };
                    self.functions.push(function);
                    self.emit(rest)
                }
                (Pattern::Var(_name), _value) => {
                    todo!()
                }
                _ => todo!(),
            },
            Expr::Int(i) => {
                self.code.push(WasmCode::Const(i.into()));
                Ok(())
            }
            Expr::IntBinOp(op, a, b) => match op {
                IntBinOp::Plus => {
                    self.emit(a)?;
                    self.emit(b)?;
                    self.code.push(WasmCode::Add(op.output_ty().into()));
                    Ok(())
                }
                IntBinOp::Minus => todo!(),
                IntBinOp::Multiply => todo!(),
                IntBinOp::Divide => todo!(),
                IntBinOp::LessThan => todo!(),
                IntBinOp::LessThanOrEqual => todo!(),
                IntBinOp::GreaterThan => todo!(),
                IntBinOp::GreaterThanOrEqual => todo!(),
            },
            Expr::Var(name) => {
                self.code.push(WasmCode::LocalGet(name, ty));
                Ok(())
            }
            Expr::Call(fun, args) => match *fun.expr {
                Expr::Var(name) => {
                    for arg in args {
                        self.emit(arg)?;
                    }
                    self.code.push(WasmCode::Call(name, ty));
                    Ok(())
                }
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

pub const LOAD_NAME: &str = ".load";

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(module")?;
        for function in &self.functions {
            write!(f, "\n{}", function)?;
        }
        write!(f, "\n (func ${} ", LOAD_NAME)?;
        if let Some(result) = self.code.last().map(|code| code.ty()) {
            write!(f, "(result {})", result)?;
        }
        for code in self.code.iter() {
            write!(f, "\n    {}", code)?;
        }
        write!(f, ")")?;
        write!(f, "\n  (export \"{}\" (func ${})))", LOAD_NAME, LOAD_NAME)
    }
}
