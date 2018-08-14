module TestInfer

open System
open System.IO

open Xunit
open Xunit.Abstractions

open Eval
open Expr
open TestUtil
open Util

type Result =
    | OK of Ty
    | Fail of string option

let fail = Fail None
let error msg = Fail (Some msg)

let tvar var = TVar {contents = var}
let gen s = 

let tests = [
    ("x", error "variable x not found");
    ("let x = x in x", error "variable x not found");
    ("let x = fun y -> y in x", OK (TArrow(tvar (Generic 0), tvar (Generic 0))));
    ("fun x -> x", OK (TArrow (tvar (Generic 0), tvar (Generic 0))));
    ("fun x -> let y = fun z -> z in y", OK (TArrow (tvar (Generic 0), TArrow (tvar (Generic 1), tvar (Generic 1)))));
    ("fun x -> let y = x in y", OK (TArrow (tvar (Generic 0), tvar (Generic 0))));
    ("fun x -> let y = let z = x(fun x -> x) in z in y", OK "forall[a b] ((a -> a) -> b) -> b");
    ("fun x -> fun y -> let x = x(y) in x(y)", OK "forall[a b] (a -> a -> b) -> a -> b");
    ("fun x -> let y = fun z -> x(z) in y", OK "forall[a b] (a -> b) -> a -> b");
    ("fun x -> let y = fun z -> x in y", OK "forall[a b] a -> b -> a");
    ("fun x -> fun y -> let x = x(y) in fun x -> y(x)",
        OK "forall[a b c] ((a -> b) -> c) -> (a -> b) -> a -> b");
    ("fun x -> let y = x in y(y)", error "recursive types");
    ("fun x -> let y = fun z -> z in y(y)", OK "forall[a b] a -> b -> b");
    ("fun x -> x(x)", error "recursive types");
    ("one(id)", error "expected a function");
    ("fun f -> let x = fun g y -> let _ = g(y) in eq(f, g) in x",
        OK "forall[a b] (a -> b) -> (a -> b, a) -> bool");
    ("let const = fun x -> fun y -> x in const", OK "forall[a b] a -> b -> a");
    ("let apply = fun f x -> f(x) in apply", OK "forall[a b] (a -> b, a) -> b");
    ("let apply_curry = fun f -> fun x -> f(x) in apply_curry", OK "forall[a b] (a -> b) -> a -> b");
]

type TestInfer (output: ITestOutputHelper) =
    inherit MakeConsoleWork(output)

    [<Fact>]
    member x.TestInfer () =
        tests
        |> List.iter (fun (input, expected) ->
            let mutable expr = None
            let setExpr e = expr <- Some e
            let mutable ty = None
            let setTy t = ty <- Some t
            let result =
                try
                    Parser.readExpr input
                    |>! setExpr
                    |> infer
                    |>! setTy
                    |> OK
                with e ->
                    printf "TestEval: Exception: %O" e
                    Fail
            if result <> expected then
                printfn "TestEval:"
                printfn "    Input = %s" input
                match expr with
                | None -> printfn "    Expr fail"
                | Some expr -> (Expr.ToStringRaw >> printfn "    Expr = %s") expr
                match ty with
                | None -> printfn "    Ty fail"
                | Some ty -> (Ty.ToStringRaw >> printfn "    Ty = %s") ty
            Assert.StrictEqual(expected, result)
        )

