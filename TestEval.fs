module TestEval

open System
open System.IO

open Xunit
open Xunit.Abstractions

open Eval
open Expr
open TestUtil
open Util

type Result =
    | OK of Value
    | Fail

let tests = [
    ("1", OK (VInt 1));
    ("-1", OK (VInt (-1)));
    ("true", OK (VBool true));
    ("false", OK (VBool false));
    ("3.14", OK (VFloat 3.14));
    ("-3.14", OK (VFloat (-3.14)));
    ("1.", OK (VFloat 1.));
    ("let a = 1 in a", OK (VInt 1));
    ("let f = fun a -> a in f(1)", OK (VInt 1));
]

type TestEval (output: ITestOutputHelper) =
    inherit MakeConsoleWork(output)

    [<Fact>]
    member x.TestEval () =
        tests
        |> List.iter (fun (input, expected) ->
            let mutable expr = None
            let setExpr e = expr <- Some e
            let mutable value = None
            let setValue v = value <- Some v
            let result =
                try
                    Parser.readExpr input
                    |>! setExpr
                    |> eval
                    |>! setValue
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
                match value with
                | None -> printfn "    Value fail"
                | Some value -> (Value.ToStringRaw >> printfn "    Value = %s") value
            Assert.StrictEqual(expected, result)
        )
