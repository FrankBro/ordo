use crate::expr::At;
use crate::expr::Expr;
use crate::expr::ExprOnly;
use crate::expr::IntBinOp;
use crate::expr::PatternOnly;
use crate::expr::Position;
use crate::lexer::Token;
use crate::parser::Error;
use crate::parser::Parser;

pub fn pvar(var: &str) -> PatternOnly {
    Expr::Var(var.to_owned()).into()
}

pub fn precord(labels: Vec<(&str, PatternOnly)>) -> PatternOnly {
    let labels = labels
        .into_iter()
        .map(|(label, pattern)| (label.to_owned(), pattern))
        .collect();
    Expr::RecordExtend(labels, Expr::RecordEmpty.into()).into()
}

pub fn bool(b: bool) -> ExprOnly {
    Expr::Bool(b).into()
}

pub fn int(i: i64) -> ExprOnly {
    Expr::Int(i).into()
}

pub fn var(var: &str) -> ExprOnly {
    Expr::Var(var.to_owned()).into()
}

pub fn call(fn_expr: ExprOnly, args: Vec<ExprOnly>) -> ExprOnly {
    Expr::Call(fn_expr, args).into()
}

pub fn fun(args: Vec<PatternOnly>, body: ExprOnly) -> ExprOnly {
    Expr::Fun(args, body).into()
}

pub fn let_(var: PatternOnly, value: ExprOnly, body: ExprOnly) -> ExprOnly {
    Expr::Let(var, value, body).into()
}

pub fn empty() -> ExprOnly {
    Expr::RecordEmpty.into()
}

pub fn select(r: ExprOnly, label: &str) -> ExprOnly {
    Expr::RecordSelect(r, label.to_owned()).into()
}

pub fn restrict(r: ExprOnly, label: &str) -> ExprOnly {
    Expr::RecordRestrict(r, label.to_owned()).into()
}

pub fn record(labels: Vec<(&str, ExprOnly)>, r: ExprOnly) -> ExprOnly {
    let labels = labels
        .into_iter()
        .map(|(label, expr)| (label.to_owned(), expr))
        .collect();
    Expr::RecordExtend(labels, r).into()
}

pub fn plus(lhs: ExprOnly, rhs: ExprOnly) -> ExprOnly {
    Expr::IntBinOp(IntBinOp::Plus, lhs, rhs).into()
}

pub fn minus(lhs: ExprOnly, rhs: ExprOnly) -> ExprOnly {
    Expr::IntBinOp(IntBinOp::Minus, lhs, rhs).into()
}

pub fn multiply(lhs: ExprOnly, rhs: ExprOnly) -> ExprOnly {
    Expr::IntBinOp(IntBinOp::Multiply, lhs, rhs).into()
}

pub fn divide(lhs: ExprOnly, rhs: ExprOnly) -> ExprOnly {
    Expr::IntBinOp(IntBinOp::Divide, lhs, rhs).into()
}

pub fn negate(expr: ExprOnly) -> ExprOnly {
    Expr::Negate(expr).into()
}

pub fn equalequal(lhs: ExprOnly, rhs: ExprOnly) -> ExprOnly {
    Expr::EqualEqual(lhs, rhs).into()
}

pub fn gt(lhs: ExprOnly, rhs: ExprOnly) -> ExprOnly {
    Expr::IntBinOp(IntBinOp::GreaterThan, lhs, rhs).into()
}

pub fn match_(
    val: ExprOnly,
    cases: Vec<(&str, &str, ExprOnly)>,
    def: Option<(&str, ExprOnly)>,
) -> ExprOnly {
    let cases = cases
        .into_iter()
        .map(|(variant, var, body)| (variant.to_owned(), var.to_owned(), body))
        .collect();
    let def = def.map(|(var, body)| (var.to_owned(), body));
    Expr::Case(val, cases, def).into()
}

pub fn if_(
    if_expr: ExprOnly,
    if_body: ExprOnly,
    elifs: Vec<(ExprOnly, ExprOnly)>,
    else_body: ExprOnly,
) -> ExprOnly {
    Expr::If(if_expr, if_body, elifs, else_body).into()
}

#[track_caller]
fn pass(source: &str, expected: ExprOnly) {
    let actual = Parser::expr(source).unwrap();
    let actual = actual.strip_context();
    assert_eq!(expected, actual);
}

#[track_caller]
fn fail(source: &str, expected: Error) {
    let actual = Parser::expr(source).unwrap_err();
    assert_eq!(expected, actual, "for {}", source);
}

#[track_caller]
fn pass_repl(source: &str, expected: ExprOnly) {
    let actual = Parser::repl(source).unwrap();
    let actual = actual.strip_context();
    assert_eq!(expected, actual);
}

#[test]
fn precedence() {
    pass("1 + 2", plus(int(1), int(2)));
    pass("1 + 2 * 3", plus(int(1), multiply(int(2), int(3))));
    pass("1 - 2 / 3", minus(int(1), divide(int(2), int(3))));
    pass(
        "false == !true",
        equalequal(bool(false), negate(bool(true))),
    );
    pass("1 + 2 > 2", gt(plus(int(1), int(2)), int(2)));
}

#[test]
fn ifs() {
    pass(
        "if a == b then a else b",
        if_(
            equalequal(var("a"), var("b")),
            var("a"),
            Vec::new(),
            var("b"),
        ),
    );
    pass(
        "if a == b then a elif b == c then b else c",
        if_(
            equalequal(var("a"), var("b")),
            var("a"),
            vec![(equalequal(var("b"), var("c")), var("b"))],
            var("c"),
        ),
    );
}

#[test]
fn patterns() {
    pass(
        "let {a} = {a = 1} in a",
        let_(
            precord(vec![("a", pvar("a"))]),
            record(vec![("a", int(1))], empty()),
            var("a"),
        ),
    );
    pass(
        "let f(a, b) = a + b in f(1, 2)",
        let_(
            pvar("f"),
            fun(vec![pvar("a"), pvar("b")], plus(var("a"), var("b"))),
            call(var("f"), vec![int(1), int(2)]),
        ),
    );
    pass(
        "let {x = {y = y}} = {x = {y = 1}} in y",
        let_(
            precord(vec![("x", precord(vec![("y", pvar("y"))]))]),
            record(vec![("x", record(vec![("y", int(1))], empty()))], empty()),
            var("y"),
        ),
    );
}

#[test]
fn exprs() {
    fail("", Error::UnexpectedEof);
    pass("a", var("a"));
    pass("f(x, y)", call(var("f"), vec![var("x"), var("y")]));
    pass(
        "f(x)(y)",
        call(call(var("f"), vec![var("x")]), vec![var("y")]),
    );
    pass(
        "let f = fun(x, y) -> g(x, y) in f(a, b)",
        let_(
            pvar("f"),
            fun(
                vec![pvar("x"), pvar("y")],
                call(var("g"), vec![var("x"), var("y")]),
            ),
            call(var("f"), vec![var("a"), var("b")]),
        ),
    );
    pass(
        "let x = a in
                let y = b in
                f(x, y)",
        let_(
            pvar("x"),
            var("a"),
            let_(
                pvar("y"),
                var("b"),
                call(var("f"), vec![var("x"), var("y")]),
            ),
        ),
    );
    fail("f x", Error::ExpectedEof(Token::Ident("x".to_owned())));
    fail(
        "let a = one",
        Error::Expected("let expr", vec![Token::In], None),
    );
    fail("a, b", Error::ExpectedEof(Token::Comma));
    fail("a = b", Error::ExpectedEof(Token::Equal));
    // TODO: Not an ideal error here
    fail("()", Error::InvalidPrefix(Some(Token::RParen)));
    pass("fun(x) -> x", fun(vec![pvar("x")], var("x")));
    // records
    pass("{}", empty());
    pass("{ }", empty());
    fail(
        "{",
        Error::Expected("record expr label", vec![Token::empty_ident()], None),
    );
    pass("a.x", select(var("a"), "x"));
    pass("m \\ a", restrict(var("m"), "a"));
    pass("{a = x}", record(vec![("a", var("x"))], empty()));
    fail(
        "{a = x",
        Error::Expected(
            "record expr",
            vec![Token::Pipe, Token::Comma, Token::RBrace],
            None,
        ),
    );
    pass(
        "{a=x, b = y}",
        record(vec![("a", var("x")), ("b", var("y"))], empty()),
    );
    pass(
        "{b = y ,a=x}",
        record(vec![("a", var("x")), ("b", var("y"))], empty()),
    );
    pass(
        "{a=x,h=w,d=y,b=q,g=z,c=t,e=s,f=r}",
        record(
            vec![
                ("a", var("x")),
                ("b", var("q")),
                ("c", var("t")),
                ("d", var("y")),
                ("e", var("s")),
                ("f", var("r")),
                ("g", var("z")),
                ("h", var("w")),
            ],
            empty(),
        ),
    );
    pass("{a = x|m}", record(vec![("a", var("x"))], var("m")));
    fail(
        "{|m}",
        Error::Expected(
            "record expr label",
            vec![Token::empty_ident()],
            Some(Token::Pipe),
        ),
    );
    pass(
        "{ a = x, b = y | m}",
        record(vec![("a", var("x")), ("b", var("y"))], var("m")),
    );
    pass(
        "{ a = x, b = y | m \\ a }",
        record(
            vec![("a", var("x")), ("b", var("y"))],
            restrict(var("m"), "a"),
        ),
    );
    pass(
        "let x = {a = f(x), b = y.b} in { a = fun(z) -> z | x \\ a }",
        let_(
            pvar("x"),
            record(
                vec![
                    ("a", call(var("f"), vec![var("x")])),
                    ("b", select(var("y"), "b")),
                ],
                empty(),
            ),
            record(
                vec![("a", fun(vec![pvar("z")], var("z")))],
                restrict(var("x"), "a"),
            ),
        ),
    );
    fail(
        "{a = x, a = y}",
        Error::DuplicateLabel(At {
            start: Position { line: 0, column: 8 },
            end: Position { line: 0, column: 9 },
            value: "a".to_owned(),
        }),
    );
    pass(
        "{x,y}",
        record(vec![("x", var("x")), ("y", var("y"))], empty()),
    );
    pass(
        "f({x,y})",
        call(
            var("f"),
            vec![record(vec![("x", var("x")), ("y", var("y"))], empty())],
        ),
    );
    pass("-9223372036854775807", int(i64::MIN + 1));
    fail("-9223372036854775808", Error::Lexer);
    pass("9223372036854775807", int(i64::MAX));
    fail("9223372036854775808", Error::Lexer);
}

#[test]
fn repl() {
    pass_repl("let a = 0", let_(pvar("a"), int(0), var("a")));
    pass_repl(
        "let f(x) = x",
        let_(pvar("f"), fun(vec![pvar("x")], var("x")), var("f")),
    );
    pass_repl(
        "let {x = x} = {x = 1}",
        let_(
            precord(vec![("x", pvar("x"))]),
            record(vec![("x", int(1))], empty()),
            record(vec![("x", var("x"))], empty()),
        ),
    );
    pass_repl(
        "let f({ x = x }) = x",
        let_(
            pvar("f"),
            fun(vec![precord(vec![("x", pvar("x"))])], var("x")),
            var("f"),
        ),
    );
    pass_repl(
            "let default_with(default, value) = match value { :some value -> value, :none x -> default }",
            let_(
                pvar("default_with"),
                fun(vec![pvar("default"), pvar("value")],
                match_(var("value"), vec![
                    ("some", "value", var("value")),
                    ("none", "x", var("default"))
                ], None)),
                var("default_with")));
}
