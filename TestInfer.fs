module TestInfer

open System
open System.IO

open Xunit
open Xunit.Abstractions

open Expr
open Infer
open Util

type Result =
    | OK of Ty
    | Fail of string option

let fail = Fail None
let error msg = Fail (Some msg)

let tvar var = TVar {contents = var}
let gen (c: char) = tvar (Generic (int c - 97))

let tests = [
    ("x", error "variable x not found");
    ("let x = x in x", error "variable x not found");
    ("let x = fun y -> y in x", OK (TArrow(gen 'a', gen 'a')));
    ("fun x -> x", OK (TArrow (gen 'a', gen 'a')));
    ("fun x -> let y = fun z -> z in y", OK (TArrow (gen 'a', TArrow (gen 'b', gen 'b'))));
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
]

type TestInfer (output: ITestOutputHelper) =

    //[<Fact>]
    member x.TestInfer () =
        tests
        |> List.iter (fun (input, expected) ->
            let result =
                try
                    Parser.readExpr input
                    |> infer
                    |> OK
                with 
                | Infer.Error msg ->
                    Fail (Some msg)
                | e ->
                    Fail None
            if result <> expected then
                output.WriteLine(sprintf "TestInfer: %s failed" input)
            Assert.StrictEqual(expected, result)
        )

