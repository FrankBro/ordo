module TestInfer

open System
open System.IO

open Xunit
open Xunit.Abstractions

open Error
open Expr
open Infer
open Util

type Result =
    | OK of Ty
    | Fail of InferError option

let tvar var = TVar {contents = var}
let gen (c: char) = tvar (Generic (int c - 97))

let fail x = Fail (Some x)

let tests = [
    ("x", fail (VariableNotFound "x"));
    ("let x = x in x", fail (VariableNotFound "x"));
    ("let x = fun y -> y in x", OK (TArrow(gen 'b', gen 'b')));
    ("fun x -> x", OK (TArrow (gen 'a', gen 'a')));
    ("fun x -> let y = fun z -> z in y", OK (TArrow (gen 'a', TArrow (gen 'c', gen 'c'))));
    ("fun x -> let y = x in y", OK (TArrow (gen 'a', gen 'a')));
    // ("fun x -> let y = let z = x(fun x -> x) in z in y", OK "forall[a b] ((a -> a) -> b) -> b");
    // ("fun x -> fun y -> let x = x(y) in x(y)", OK "forall[a b] (a -> a -> b) -> a -> b");
    // ("fun x -> let y = fun z -> x(z) in y", OK "forall[a b] (a -> b) -> a -> b");
    // ("fun x -> let y = fun z -> x in y", OK "forall[a b] a -> b -> a");
    // ("fun x -> fun y -> let x = x(y) in fun x -> y(x)",
    //     OK "forall[a b c] ((a -> b) -> c) -> (a -> b) -> a -> b");
    // ("fun x -> let y = x in y(y)", error "recursive types");
    // ("fun x -> let y = fun z -> z in y(y)", OK "forall[a b] a -> b -> b");
    // ("fun x -> x(x)", error "recursive types");
    // ("one(id)", error "expected a function");
    // ("fun f -> let x = fun g y -> let _ = g(y) in eq(f, g) in x",
    //     OK "forall[a b] (a -> b) -> (a -> b, a) -> bool");
    // ("let const = fun x -> fun y -> x in const", OK "forall[a b] a -> b -> a");
    // ("let apply = fun f x -> f(x) in apply", OK "forall[a b] (a -> b, a) -> b");
    // ("let apply_curry = fun f -> fun x -> f(x) in apply_curry", OK "forall[a b] (a -> b) -> a -> b");
    (* records *)
    ("{}", OK (TRecord TRowEmpty));
    ("{}.x", Fail None); // UnifyFail
    ("{a = 1}", OK (TRecord (TRowExtend ("a", TConst "int", TRowEmpty))));
    // ("{a = one, b = true}", OK "{a : int, b : bool}");
    // ("{b = true, a = one}", OK "{b : bool, a : int}");
    // ("{a = one, b = true}.a", OK "int");
    // ("{a = one, b = true}.b", OK "bool");
    // ("{a = one, b = true}.c", error "row does not contain label c");
    ("{f = fun x -> x}", OK (TRecord (TRowExtend ("f", TArrow (gen 'c', gen 'c'), TRowEmpty))))
    // ("let r = {a = id, b = succ} in choose(r.a, r.b)", OK "int -> int");
    // ("let r = {a = id, b = fun x -> x} in choose(r.a, r.b)", OK "forall[a] a -> a");
    // ("choose({a = one}, {})", fail);
    // ("{ x = zero | { y = one | {} } }", OK "{x : int, y : int}");
    // ("choose({ x = zero | { y = one | {} } }, {x = one, y = zero})", OK "{x : int, y : int}");
    ("{{} - x}", Fail None); // UnifyFail
    // ("{{x = one, y = zero} - x}", OK "{y : int}");
    // ("{ x = true | {x = one}}", OK "{x : bool, x : int}");
    // ("let a = {} in {b = one | a}", OK "{b : int}");
    // ("let a = {x = one} in {x = true | a}.x", OK "bool");
    // ("let a = {x = one} in a.y", error "row does not contain label y");
    // ("let a = {x = one} in {a - x}", OK "{}");
    // ("let a = {x = one} in let b = {x = true | a} in {b - x}.x", OK "int");
    // ("fun r -> {x = one | r}", OK "forall[r] {r} -> {x : int | r}");
    // ("fun r -> r.x", OK "forall[r a] {x : a | r} -> a");
    // ("let get_x = fun r -> r.x in get_x({y = one, x = zero})", OK "int");
    // ("let get_x = fun r -> r.x in get_x({y = one, z = true})", error "row does not contain label x");
    // ("fun r -> choose({x = zero | r}, {x = one | {}})", OK "{} -> {x : int}");
    // ("fun r -> choose({x = zero | r}, {x = one})", OK "{} -> {x : int}");
    // ("fun r -> choose({x = zero | r}, {x = one | r})", OK "forall[r] {r} -> {x : int | r}");
    // ("fun r -> choose({x = zero | r}, {y = one | r})", error "recursive row types");
    // ("let f = fun x -> x.t(one) in f({t = succ})", OK "int");
    // ("let f = fun x -> x.t(one) in f({t = id})", OK "int");
    // ("{x = one, x = true}", OK "{x : int, x : bool}");

    // ("let f = fun r -> let y = r.y in choose(r, {x = one, x = true}) in f",
    // 	error "row does not contain label y");
    // ("fun r -> let y = choose(r.x, one) in let z = choose({r - x}.x, true) in r",
    // 	OK "forall[a r] {x : int, x : bool | r} -> {x : int, x : bool | r}");
    // ("fun r -> choose({x = zero | r}, {x = one, x = true})", OK "{x : bool} -> {x : int, x : bool}");
    // ("fun r -> choose(r, {x = one, x = true})", OK "{x : int, x : bool} -> {x : int, x : bool}");
    // ("fun r -> choose({x = zero | r}, {x = true | r})", error "cannot unify types int and bool");
    // ("fun r s -> " ^
    //  "choose({b = true, a = one, c = zero, b = half | r}, {b = false, c = one, d = half | s})",
    // 	OK ("forall[a] ({d : float | a}, {a : int, b : float | a}) -> " ^
    // 	    "{a : int, b : bool, b : float, c : int, d : float | a}"));
    // ("fun r s -> choose({b = true, a = one, c = zero, b = half | r}, {b = false, c = one | s})",
    // 	OK "forall[a] ({a}, {a : int, b : float | a}) -> {a : int, b : bool, b : float, c : int | a}");
    // ("fun r s -> choose({b = true, c = zero | r}, {b = false, c = one, d = half | s})",
    // 	OK "forall[a] ({d : float | a}, {a}) -> {b : bool, c : int, d : float | a}");
    // ("fun r s -> " ^
    //  "choose({b = true, a = one, c = one, b = half | r}, {b = false, c = one, a = one, b = half | s})",
    // 	OK "forall[a] ({a}, {a}) -> {a : int, b : bool, b : float, c : int | a}");
    // ("fun r -> {x = r | r}", OK "forall[a] {a} -> {x : {a} | a}");

    // (* variants *)
    // (":x 1", OK (TVariant (TRowExtend (Map.singleton "x" [TConst "int"], gen 'a'))))
    // ("choose(choose(:x one, :Y true), choose(:X half, :y nil))",
    // 	OK "forall[a b] [X : float, Y : bool, x : int, y : list[a] | b]");
    // ("choose(:X one, :X true)", error "cannot unify types int and bool");
    // ("choose(:X {x = one, y = false}, :Y {w = half})",
    // 	OK "forall[a] [X : {x : int, y : bool}, Y : {w : float} | a]");
    // ("let e = choose(choose(:x one, :Y true), choose(:X half, :y nil)) in " ^
    //  "match e { :x i -> i | :Y y -> zero}", error "row does not contain label X");
    // ("fun x y -> match x {:a i -> one | :b i -> zero | :c i -> y}",
    // 	OK "forall [a b c] ([a : a, b : b, c : c], int) -> int");
    // ("fun a -> match a {:X i -> i | r -> one}", OK "forall[a] [X : int | a] -> int");
    // ("let f = fun m -> match m {:y a -> one | :Y b -> zero | :z z -> zero} in " ^
    //  "fun e -> match e { :x i -> i | :X f -> one | r -> f(r)}",
    //  OK "forall[a b c d] [X : a, Y : b, x : int, y : c, z : d] -> int");
    // ("let e = choose(choose(:x one, :Y true), choose(:X half, :y nil)) in " ^
    //  "let f = fun m -> match m {:y a -> one | :Y b -> zero | :z z -> zero} in " ^
    //  "match e { :x i -> i | :X f -> one | r -> f(r)}", OK "int");
    // ("fun e -> match e {:X a -> one | :X i -> i}", OK "forall[a] [X : a, X : int] -> int");
    // ("let f = fun g -> fun e -> match e { :x i -> i | :Y a -> one | r -> g(r)} in " ^
    //  "let g = fun s -> match s {:x j -> head(j) | :X a -> zero} in " ^
    //  "f(g)", OK "forall[a b] [X : a, Y : b, x : int, x : list[int]] -> int");
    // ("fun e -> match e { :X a -> plus(a.x, one) }", OK "forall[a] [X : {x : int | a}] -> int");
    // ("let count1 = fun count -> fun x -> " ^
    //  " match x {:Cons a -> plus(one, count(a.tail)) | :Nil _ -> zero} in " ^
    //  "fix(count1)", error "recursive types");
]

type TestInfer (output: ITestOutputHelper) =

    [<Fact>]
    member x.TestInfer () =
        tests
        |> List.iter (fun (input, expected) ->
            let result =
                try
                    Parser.readExpr input
                    |> infer
                    |> OK
                with 
                | ErrorException (UnifyFail _) ->
                    // A mess to make match
                    Fail None
                | ErrorException error ->
                    Fail (Some error)
                | e ->
                    Fail None
            if result <> expected then
                output.WriteLine(sprintf "TestInfer: %s failed" input)
                match result with
                | OK result ->
                    output.WriteLine(sprintf "Result: %s" (string result))
                | Fail e -> 
                    output.WriteLine(sprintf "Result: Fail %O" e)
                match expected with
                | OK expected ->
                    output.WriteLine(sprintf "Expected: %s" (string expected))
                | Fail e -> 
                    output.WriteLine(sprintf "Result: Fail %O" e)
            Assert.StrictEqual(expected, result)
        )
