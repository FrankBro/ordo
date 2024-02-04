use crate::{
    expr::{Expr, ExprAt, IntBinOp, PatternAt, Position, PositionContext},
    parser::Parser,
};

pub fn int_at(i: i64, context: PositionContext) -> ExprAt {
    ExprAt {
        context,
        expr: Expr::Int(i).into(),
    }
}

pub fn var_at(var: &str, context: PositionContext) -> ExprAt {
    ExprAt {
        context,
        expr: Expr::Var(var.to_owned()).into(),
    }
}

pub fn let_at(var: PatternAt, value: ExprAt, body: ExprAt, context: PositionContext) -> ExprAt {
    ExprAt {
        context,
        expr: Expr::Let(var, value, body).into(),
    }
}

pub fn plus_at(lhs: ExprAt, rhs: ExprAt, context: PositionContext) -> ExprAt {
    ExprAt {
        context,
        expr: Expr::IntBinOp(IntBinOp::Plus, lhs, rhs).into(),
    }
}

fn pass_at(source: &str, expected: ExprAt) {
    let actual = Parser::expr(source).unwrap();
    assert_eq!(expected, actual);
}

fn loc(sl: usize, sc: usize, el: usize, ec: usize) -> PositionContext {
    PositionContext {
        start: Position {
            line: sl,
            column: sc,
        },
        end: Position {
            line: el,
            column: ec,
        },
    }
}

#[test]
fn at() {
    pass_at("a", var_at("a", loc(0, 0, 0, 1)));
    pass_at("1", int_at(1, loc(0, 0, 0, 1)));
    pass_at(
        "1 + 2",
        plus_at(
            int_at(1, loc(0, 0, 0, 1)),
            int_at(2, loc(0, 4, 0, 5)),
            loc(0, 2, 0, 3),
        ),
    );
    pass_at(
        "let a = 1 in\na",
        let_at(
            var_at("a", loc(0, 4, 0, 5)),
            int_at(1, loc(0, 8, 0, 9)),
            var_at("a", loc(1, 0, 1, 1)),
            loc(0, 0, 0, 3),
        ),
    );
    pass_at("10", int_at(10, loc(0, 0, 0, 2)));
}
