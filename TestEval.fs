module TestEval

open System
open System.IO

open Xunit
open Xunit.Abstractions

open Eval
open Error
open Expr
open Util

type Result =
    | OK of Value
    | Fail of OrdoError option

let record fields = VRecord (Map.ofList fields)

let fail e = Fail (Some e)

let tests = [
    ("1", OK (VInt 1));
    ("-1", OK (VInt (-1)));
    ("true", OK (VBool true));
    ("false", OK (VBool false));
    ("3.14", OK (VFloat 3.14));
    ("-3.14", OK (VFloat (-3.14)));
    ("1.", OK (VFloat 1.));
    ("let a = 1 in a", OK (VInt 1));
    ("let f = fun a -> a in f 1", OK (VInt 1));
    ("{}", OK (VRecord Map.empty))
    ("{ a = 1 }", OK (record ["a", VInt 1]))
    ("{ a = 1 | { b = 2 } }", OK (record ["a", VInt 1; "b", VInt 2]))
    ("{ { a = 1 } - a }", OK (VRecord Map.empty))
    ("{ a = 1 }.a", OK (VInt 1))
    (":a 1", OK (VVariant ("a", VInt 1)))
    ("match :x 1 { :x i -> i | :y i -> i }", OK (VInt 1))
    ("match :x 1 { :y i -> i | otherwise -> 0 }", OK (VInt 0))
    ("if true then 1 else 0", OK (VInt 1))
    ("if 1 then 1 else 0", fail (OrdoError.Generic IfValueNotBoolean))
    ("let { a = a } = { a = 1 } in a", OK (VInt 1))
]

type TestEval (output: ITestOutputHelper) =

    [<Fact>]
    member x.TestEval () =
        tests
        |> List.iter (fun (input, expected) ->
            let result =
                try
                    Parser.readExpr input
                    |> eval
                    |> OK
                with 
                | OrdoException e ->
                    Fail (Some e)
                | e ->
                    output.WriteLine(sprintf "Unknown exception: %O" e)
                    Fail None
            if result <> expected then
                output.WriteLine(sprintf "TestEval: %s failed" input)
            Assert.StrictEqual(expected, result)
        )
