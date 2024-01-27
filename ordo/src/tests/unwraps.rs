use std::collections::BTreeMap;

use crate::{
    eval::{self, Value},
    infer,
    parser::Parser,
};

struct Env {
    infer: infer::Env,
    eval: eval::Env,
}

impl Env {
    fn new() -> Self {
        let infer = infer::Env::default();
        let eval = eval::Env::default();
        let mut env = Self { infer, eval };
        env.add(
            "let safe_div(n, d) = if d == 0 then :div_by_zero {} else :ok (n / d)",
            "forall ra. (ra\\div_by_zero\\ok) => (int, int) -> [div_by_zero: {}, ok: int | ra]",
        );
        env.add(
            "let safe_minus(x, y) = if y < 0 then :would_add {} else :ok (x - y)",
            "forall ra. (ra\\ok\\would_add) => (int, int) -> [ok: int, would_add: {} | ra]",
        );
        env
    }

    fn add(&mut self, source: &str, source_ty: &str) {
        let (forall, ty) = Parser::ty(source_ty).unwrap();
        let expected_ty = self.infer.replace_ty_constants_with_vars(forall, ty);
        let expr = Parser::repl(source).unwrap();
        let typed_expr = self.infer.infer(expr.clone()).unwrap();
        let actual_ty = typed_expr.context.ty.ty;
        let expected_ty = self.infer.ty_to_string(&expected_ty).unwrap();
        let actual_ty = self.infer.ty_to_string(&actual_ty).unwrap();
        assert_eq!(expected_ty, actual_ty);
        let _ = self.eval.eval(&expr).unwrap();
    }
}

#[track_caller]
fn pass(source: &str, source_ty: &str, expected_val: Value) {
    let mut env = Env::new();
    let (forall, ty) = Parser::ty(source_ty).unwrap();
    let expected_ty = env.infer.replace_ty_constants_with_vars(forall, ty);
    let expr = Parser::expr(source).unwrap();
    let typed_expr = env.infer.infer(expr.clone()).unwrap();
    let actual_ty = typed_expr.context.ty.ty;
    let expected_ty = env.infer.ty_to_string(&expected_ty).unwrap();
    let actual_ty = env.infer.ty_to_string(&actual_ty).unwrap();
    assert_eq!(expected_ty, actual_ty);
    let actual_val = env.eval.eval(&expr).unwrap();
    assert_eq!(expected_val, actual_val);
}

#[test]
fn unwrap_ok() {
    pass(
        "safe_div(2, 2)?",
        "forall ra. (ra\\div_by_zero\\ok) => [div_by_zero: {}, ok: int | ra]",
        Value::Int(1),
    );
    pass(
        "safe_div(2, 0)?",
        "forall ra. (ra\\div_by_zero\\ok) => [div_by_zero: {}, ok: int | ra]",
        Value::Variant(
            "div_by_zero".to_owned(),
            Value::Record(BTreeMap::new()).into(),
        ),
    );
    pass(
        "let x = safe_div(2, 2)? in x",
        "forall ra. (ra\\div_by_zero\\ok) => [div_by_zero: {}, ok: int | ra]",
        Value::Int(1),
    );
    pass(
        "let x = safe_div(2, 0)? in x",
        "forall ra. (ra\\div_by_zero\\ok) => [div_by_zero: {}, ok: int | ra]",
        Value::Variant("div_by_zero".to_owned(), Value::record(Vec::new()).into()),
    );
    pass(
        "let x = safe_div(2, 2)? in let y = safe_minus(x, 1)? in y",
        "forall ra. (ra\\div_by_zero\\ok\\would_add) => [div_by_zero: {}, ok: int, would_add: {} | ra]",
        Value::Int(0),
    );
}
