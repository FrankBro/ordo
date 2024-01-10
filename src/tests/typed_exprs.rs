use crate::{
    core::make_env,
    expr::{Expr, ExprIn, ExprTyped, Type, TypeContext},
    parser::Parser,
};

#[track_caller]
fn pass(expr_str: &str, expected: ExprTyped) {
    let mut env = make_env();
    let expr = Parser::expr(expr_str).unwrap();
    let actual = env.infer(expr).unwrap();
    let actual = actual.strip_position();
    assert_eq!(expected, actual, "for {}", expr_str);
}

fn expr(expr: Expr<TypeContext>, ty: Type) -> ExprTyped {
    let context = TypeContext { ty };
    ExprIn { context, expr }
}

fn int(i: i64) -> ExprTyped {
    expr(Expr::Int(i), Type::int())
}

fn var(name: &str, ty: Type) -> ExprTyped {
    expr(Expr::Var(name.to_owned()), ty)
}

fn let_(pattern: ExprTyped, value: ExprTyped, body: ExprTyped) -> ExprTyped {
    let ty = body.context.ty.clone();
    expr(Expr::Let(pattern.into(), value.into(), body.into()), ty)
}

#[test]
fn tests() {
    pass(
        "let x = 1 in x",
        let_(var("x", Type::int()), int(1), var("x", Type::int())),
    );
}
