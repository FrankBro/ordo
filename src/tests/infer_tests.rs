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
fn infer_pattern() {
    pass("let x = 1 in x", "int");
    pass("let {x = x} = {x = 1} in x", "int");
    pass("let {x} = {x = 1} in x", "int");
    pass("let {x = {y = y}} = {x = {y = 1}} in y", "int");
}

#[test]
fn infer_base() {
    pass("id", "forall a => a -> a");
    pass("one", "int");
    fail("x", Error::VariableNotFound("x".to_owned()));
    fail("let x = x in x", Error::VariableNotFound("x".to_owned()));
    pass("let x = id in x", "forall a => a -> a");
    pass("let x = fun(y) -> y in x", "forall a => a -> a");
    pass("fun(x) -> x", "forall a => a -> a");
    pass("pair", "forall a b => (a, b) -> pair[a, b]");
    pass("pair", "forall z x => (x, z) -> pair[x, z]");
    pass(
        "fun(x) -> let y = fun(z) -> z in y",
        "forall a b => a -> b -> b",
    );
    pass(
        "let f = fun(x) -> x in let id = fun(y) -> y in eq(f, id)",
        "bool",
    );
    pass(
        "let f = fun(x) -> x in let id = fun(y) -> y in eq_curry(f)(id)",
        "bool",
    );
    pass("let f = fun(x) -> x in eq(f, succ)", "bool");
    pass("let f = fun(x) -> x in eq_curry(f)(succ)", "bool");
    pass(
        "let f = fun(x) -> x in pair(f(one), f(true))",
        "pair[int, bool]",
    );
    fail(
        "fun(f) -> pair(f(one), f(true))",
        Error::CannotUnify("bool".to_owned(), "int".to_owned()),
    );
    pass(
        "let f = fun(x, y) -> let a = eq(x, y) in eq(x, y) in f",
        "forall a => (a, a) -> bool",
    );
    pass(
        "let f = fun(x, y) -> let a = eq_curry(x)(y) in eq_curry(x)(y) in f",
        "forall a => (a, a) -> bool",
    );
    pass("id(id)", "forall a => a -> a");
    pass(
        "choose(fun(x, y) -> x, fun(x, y) -> y)",
        "forall a => (a, a) -> a",
    );
    pass(
        "choose_curry(fun(x, y) -> x)(fun(x, y) -> y)",
        "forall a => (a, a) -> a",
    );
    pass(
        "let x = id in let y = let z = x(id) in z in y",
        "forall a => a -> a",
    );
    pass("cons(id, nil)", "forall a => list[a -> a]");
    pass("cons_curry(id)(nil)", "forall a => list[a -> a]");
    pass(
        "let lst1 = cons(id, nil) in let lst2 = cons(succ, lst1) in lst2",
        "list[int -> int]",
    );
    pass(
        "cons_curry(id)(cons_curry(succ)(cons_curry(id)(nil)))",
        "list[int -> int]",
    );
    fail(
        "plus(one, true)",
        Error::CannotUnify("bool".to_owned(), "int".to_owned()),
    );
    fail("plus(one)", Error::UnexpectedNumberOfArguments);
    pass("fun(x) -> let y = x in y", "forall a => a -> a");
    pass(
        "fun(x) -> let y = let z = x(fun(x) -> x) in z in y",
        "forall a b => ((a -> a) -> b) -> b",
    );
    pass(
        "fun(x) -> fun(y) -> let x = x(y) in x(y)",
        "forall a b => (a -> a -> b) -> a -> b",
    );
    pass(
        "fun(x) -> let y = fun(z) -> x(z) in y",
        "forall a b => (a -> b) -> a -> b",
    );
    pass(
        "fun(x) -> let y = fun(z) -> x in y",
        "forall a b => a -> b -> a",
    );
    pass(
        "fun(x) -> fun(y) -> let x = x(y) in fun(x) -> y(x)",
        "forall a b c => ((a -> b) -> c) -> (a -> b) -> a -> b",
    );
    fail("fun(x) -> let y = x in y(y)", Error::RecursiveType);
    pass(
        "fun(x) -> let y = fun(z) -> z in y(y)",
        "forall a b => a -> b -> b",
    );
    fail("fun(x) -> x(x)", Error::RecursiveType);
    fail("one(id)", Error::ExpectedFunction("int".to_owned()));
    pass(
        "fun(f) -> let x = fun(g, y) -> let _ = g(y) in eq(f, g) in x",
        "forall a b => (a -> b) -> (a -> b, a) -> bool",
    );
    pass(
        "let const = fun(x) -> fun(y) -> x in const",
        "forall a b => a -> b -> a",
    );
    pass(
        "let apply = fun(f, x) -> f(x) in apply",
        "forall a b => (a -> b, a) -> b",
    );
    pass(
        "let apply_curry = fun(f) -> fun(x) -> f(x) in apply_curry",
        "forall a b => (a -> b) -> a -> b",
    );
    pass("1 == 1", "bool");
    pass("1 > 2", "bool");
}

#[test]
fn infer_records() {
    pass("{}", "{}");
    fail("{}.x", Error::MissingLabel("x".to_owned()));
    pass("{a = one}", "{a : int}");
    pass("{a = one, b = true}", "{a : int, b : bool}");
    pass("{b = true, a = one}", "{b : bool, a : int}");
    pass("{a = one, b = true}.a", "int");
    pass("{a = one, b = true}.b", "bool");
    fail("{a = one, b = true}.c", Error::MissingLabel("c".to_owned()));
    pass("{f = fun(x) -> x}", "forall a => {f : a -> a}");
    pass(
        "let r = {a = id, b = succ} in choose(r.a, r.b)",
        "int -> int",
    );
    pass(
        "let r = {a = id, b = fun(x) -> x} in choose(r.a, r.b)",
        "forall a => a -> a",
    );
    fail("choose({a = one}, {})", Error::MissingLabel("a".to_owned()));
    pass("{ x = zero | { y = one | {} } }", "{y : int, x : int}");
    pass(
        "choose({ x = zero | { y = one | {} } }, {x = one, y = zero})",
        "{y : int, x : int}",
    );
    fail("{}\\x", Error::MissingLabel("x".to_owned()));
    pass("{x = one, y = zero} \\ x", "{y : int}");
    pass("{ x = true | {x = one}\\x}", "{x : bool}");
    pass("let a = {} in {b = one | a}", "{b : int}");
    pass("let a = {x = one} in {x = true | a\\x}.x", "bool");
    fail(
        "let a = {x = one} in a.y",
        Error::MissingLabel("y".to_owned()),
    );
    pass("let a = {x = one} in a \\ x", "{}");
    fail(
        "let a = {x = one} in let b = {x = true | a\\x} in b\\x.x",
        Error::MissingLabel("x".to_owned()),
    );
    pass(
        "fun(r) -> {x = one | r}",
        "forall ra. (ra\\x) => {ra} -> {x : int | ra}",
    );
    pass("fun(r) -> r.x", "forall ra a. (ra\\x) => {x : a | ra} -> a");
    pass(
        "let get_x = fun(r) -> r.x in get_x({y = one, x = zero})",
        "int",
    );
    fail(
        "let get_x = fun(r) -> r.x in get_x({y = one, z = true})",
        Error::MissingLabel("x".to_owned()),
    );
    pass(
        "fun(r) -> choose({x = zero | r}, {x = one | {}})",
        "{} -> {x : int}",
    );
    pass(
        "fun(r) -> choose({x = zero | r}, {x = one})",
        "{} -> {x : int}",
    );
    pass(
        "fun(r) -> choose({x = zero | r}, {x = one | r})",
        "forall ra. (ra\\x) => {ra} -> {x : int | ra}",
    );
    fail(
        "fun(r) -> choose({x = zero | r}, {y = one | r})",
        Error::RowConstraintFailed("y".to_owned()),
    );
    pass("let f = fun(x) -> x.t(one) in f({t = succ})", "int");
    pass("let f = fun(x) -> x.t(one) in f({t = id})", "int");
    fail(
        "let f = fun(r) -> let y = r.y in choose(r, {x = one}) in f",
        Error::MissingLabel("y".to_owned()),
    );
    fail(
        "fun(r) -> choose({x = zero | r}, {x = true | r})",
        Error::CannotUnify("int".to_owned(), "bool".to_owned()),
    );
    pass(
            "fun(r, s) -> choose({b = true, c = zero | r}, {b = false, c = one, d = half | s})",
            "forall ra. (ra\\b\\c\\d) => ({d : float | ra}, {ra}) -> {b : bool, c : int, d : float | ra}",
        );
    pass(
        "fun(r) -> {x = r | r}",
        "forall ra. (ra\\x) => {ra} -> {x : {ra} | ra}",
    );
    pass("let { x = x } = { x = 1 } in { x = x }", "{ x: int}");
}

#[test]
fn infer_variant() {
    pass(":X one", "forall ra. (ra\\X) => [X : int | ra]");
    pass(
        "choose(choose(:x one, :Y true), choose(:X half, :y nil))",
        "forall a ra. (ra\\X\\Y\\x\\y) => [X : float, Y : bool, x : int, y : list[a] | ra]",
    );
    fail(
        "choose(:X one, :X true)",
        Error::CannotUnify("int".to_owned(), "bool".to_owned()),
    );
    pass(
        "choose(:X {x = one, y = false}, :Y {w = half})",
        "forall ra. (ra\\X\\Y) => [X : {x : int, y : bool}, Y : {w : float} | ra]",
    );
    fail(
        concat!(
            "let e = choose(choose(:x one, :Y true), choose(:X half, :y nil)) in ",
            "match e { :x i -> i , :Y y -> zero}"
        ),
        Error::MissingLabel("X".to_owned()),
    );
    pass(
        "fun(x, y) -> match x {:a i -> one , :b i -> zero , :c i -> y}",
        "forall a b c => ([a : a, b : b, c : c], int) -> int",
    );
    pass(
        "fun(a) -> match a {:X i -> i , r -> one}",
        "forall ra. (ra\\X) => [X : int | ra] -> int",
    );
    pass(
        concat!(
            "let f = fun(m) -> match m {:y a -> one , :Y b -> zero , :z z -> zero} in ",
            "fun(e) -> match e { :x i -> i , :X f -> one , r -> f(r)}"
        ),
        "forall a b c d => [X : a, Y : b, x : int, y : c, z : d] -> int",
    );
    pass(
        concat!(
            "let e = choose(choose(:x one, :Y true), choose(:X half, :y nil)) in ",
            "let f = fun(m) -> match m {:y a -> one , :Y b -> zero , :z z -> zero} in ",
            "match e { :x i -> i , :X f -> one , r -> f(r)}"
        ),
        "int",
    );
    pass(
        "fun(e) -> match e { :X a -> plus(a.x, one) }",
        "forall ra. (ra\\x) => [X : {x : int | ra}] -> int",
    );
}
