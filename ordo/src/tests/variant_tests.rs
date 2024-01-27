use crate::{core::make_env, infer::Error, parser::Parser};

#[track_caller]
fn pass(expr_str: &str, expected: &str) {
    let (forall, ty) = Parser::ty(expected).unwrap();
    let mut env = make_env();
    let expected = env.replace_ty_constants_with_vars(forall, ty);
    let expr = Parser::expr(expr_str).unwrap();
    let typed_expr = env.infer(expr).unwrap();
    let actual = typed_expr.context.ty.ty;
    let expected = env.ty_to_string(&expected).unwrap();
    let actual = env.ty_to_string(&actual).unwrap();
    assert_eq!(expected, actual, "for {}", expr_str);
}

#[track_caller]
fn fail(expr_str: &str, expected: Error) {
    let mut env = make_env();
    let expr = Parser::expr(expr_str).unwrap();
    let actual = env.infer(expr).unwrap_err();
    assert_eq!(expected, actual, "for {}", expr_str);
}

#[test]
fn variant_tests() {
    pass(
        "let f(v) = match v { :a a -> 0, :b b -> 1 } in f(:a 1)",
        "int",
    );
    pass(
        "let f(v) = match v { :a a -> 0, :b b -> 1, otherwise -> 2 } in f(:a 1)",
        "int",
    );
    fail(
        "let f(v) = match v { :a a -> 0, :b b -> 1 } in f(:c 1)",
        Error::MissingLabel("c".to_owned()),
    );
    pass(
        "let f(v) = match v { :a a -> 0, :b b -> 1, otherwise -> 2 } in f(:c 1)",
        "int",
    );
}
