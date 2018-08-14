module TestParser

open System
open System.IO

open Xunit
open Xunit.Abstractions

open Expr

type Result =
    | OK of Expr
    | Fail

let tests = [
    ("", Fail);
    ("a", OK (EVar "a"));
    ("f x", OK (ECall(EVar "f", EVar "x")));
    ("f x y", OK (ECall(ECall(EVar "f", EVar "x"), EVar "y")));
    // ("let f = fun x y -> g(x, y) in f(a, b)",
    //     OK (ELet("f", EValue (VFun(["x"; "y"], ECall(EVar "g", [EVar "x"; EVar "y"]))), ECall(EVar "f", [EVar "a"; EVar "b"]))));
    // ("let x = a in " +
    //  "let y = b in " +
    //  "f(x, y)", OK (ELet("x", EVar "a", ELet("y", EVar "b", ECall(EVar "f", [EVar "x"; EVar "y"])))));
    ("let a = one", Fail);
    // ("a, b", Fail);
    ("a = b", Fail);
    ("()", Fail);
    // ("fun x, y -> y", Fail);
    ("1", OK (EValue (VInt 1)));
    ("-1", OK (EValue (VInt (-1))));
    ("true", OK (EValue (VBool true)));
    ("false", OK (EValue (VBool false)));
    ("3.14", OK (EValue (VFloat 3.14)));
    ("-3.14", OK (EValue (VFloat (-3.14))));
    ("1.", OK (EValue (VFloat 1.)));
    // ("let f = fun a -> a in f(1)", 
    //     OK (ELet("f", EValue (VFun(["a"], EVar "a")), ECall(EVar "f", [EValue (VInt 1)]))));
]

type TestParser (output: ITestOutputHelper) =

    [<Fact>]
    member x.TestParser () =
        tests
        |> List.iter (fun (input, expected) ->
            let result =
                try
                    Parser.readExpr input
                    |> OK
                with _ ->
                    Fail
            if result <> expected then
                output.WriteLine(sprintf "TestParser: %s failed" input)
            Assert.StrictEqual(expected, result)
        )
