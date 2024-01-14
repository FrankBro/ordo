use crate::{
    core::make_env,
    expr::{Expr, ExprIn, ExprTyped, IntBinOp, Type, TypeContext},
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
    let expr = Box::new(expr);
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
    expr(Expr::Let(pattern, value, body), ty)
}

fn fun(params: Vec<ExprTyped>, body: ExprTyped) -> ExprTyped {
    let params_ty = params
        .iter()
        .map(|param| param.context.ty.clone())
        .collect();
    let ty = Type::Arrow(params_ty, body.context.ty.clone().into());
    expr(Expr::Fun(params, body), ty)
}

fn add(a: ExprTyped, b: ExprTyped) -> ExprTyped {
    let op = IntBinOp::Plus;
    let ty = op.output_ty();
    expr(Expr::IntBinOp(op, a, b), ty)
}

fn call(fun: ExprTyped, args: Vec<ExprTyped>) -> ExprTyped {
    let ty = match fun.context.ty.clone() {
        Type::Arrow(_, ret) => *ret,
        _ => unreachable!(),
    };
    expr(Expr::Call(fun, args), ty)
}

#[test]
fn tests() {
    pass(
        "let x = 1 in x",
        let_(var("x", Type::int()), int(1), var("x", Type::int())),
    );
    pass(
        "let add(a, b) = a + b in add(1, 2)",
        let_(
            var(
                "add",
                Type::Arrow(vec![Type::int(), Type::int()], Type::int().into()),
            ),
            fun(
                vec![var("a", Type::int()), var("b", Type::int())],
                add(var("a", Type::int()), var("b", Type::int())),
            ),
            call(
                var(
                    "add",
                    Type::Arrow(vec![Type::int(), Type::int()], Type::int().into()),
                ),
                vec![int(1), int(2)],
            ),
        ),
    );
}
