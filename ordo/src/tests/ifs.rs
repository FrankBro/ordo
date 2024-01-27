use std::collections::BTreeMap;

use crate::{
    eval::{self, Value},
    infer,
    parser::Parser,
};

#[track_caller]
fn pass(source_expr: &str, source_ty: &str, expected_val: Value) {
    let (forall, ty) = Parser::ty(source_ty).unwrap();
    let mut env = infer::Env::default();
    let expected = env.replace_ty_constants_with_vars(forall, ty);
    let expr = Parser::expr(source_expr).unwrap();
    let typed_expr = env.infer(expr.clone()).unwrap();
    let actual = typed_expr.context.ty.ty;
    let expected_ty = env.ty_to_string(&expected).unwrap();
    let actual_ty = env.ty_to_string(&actual).unwrap();
    assert_eq!(expected_ty, actual_ty);
    let mut env = eval::Env::default();
    let actual_val = env.eval(&expr).unwrap();
    assert_eq!(expected_val, actual_val);
}

#[track_caller]
fn fail_ty(source_expr: &str, expected: infer::Error) {
    let mut env = infer::Env::default();
    let expr = Parser::expr(source_expr).unwrap();
    let actual = env.infer(expr).unwrap_err();
    assert_eq!(expected, actual);
}

#[test]
fn ifs() {
    pass("if 1 == 1 then 1 else 0", "int", Value::Int(1));
    pass("if 1 == 0 then 1 else 0", "int", Value::Int(0));
    fail_ty(
        "if 1 then 1 else 0",
        infer::Error::CannotUnify("bool".to_owned(), "int".to_owned()),
    );
    fail_ty(
        "if 1 == 0 then true else 1",
        infer::Error::CannotUnify("bool".to_owned(), "int".to_owned()),
    );
    fail_ty(
        "if 1 == 0 then 1 else true",
        infer::Error::CannotUnify("int".to_owned(), "bool".to_owned()),
    );
    pass(
        "if 1 == 1 then :one {} else :zero {}",
        "forall ra. (ra\\one\\zero) => [one: {}, zero: {} | ra]",
        Value::Variant("one".to_owned(), Value::Record(BTreeMap::new()).into()),
    );
    pass(
        "if 1 == 1 then {a = 1} else {a = 0}",
        "{a: int}",
        Value::record(vec![("a", Value::Int(1))]),
    );
    fail_ty(
        "if 1 == 1 then {a = 1} else {b = 0}",
        infer::Error::MissingLabel("b".to_owned()),
    );
}
