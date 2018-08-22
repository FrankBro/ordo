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
    // ("a = b", Fail);
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
    ("{}", OK ERecordEmpty);
    ("{ }", OK ERecordEmpty);
    ("{", Fail);
    ("a.x", OK (ERecordSelect(EVar "a", "x")));
    ("{m - a}", OK (ERecordRestrict(EVar "m", "a")));
    ("{m - a", Fail);
    // ("m - a", Fail); THIS SHOULD FAIL BUT RETURNS EVar "m"
    ("{a = x}", OK (ERecordExtend ("a", EVar "x", ERecordEmpty)))
    ("{a = x", Fail);
    ("{a=x, b = y}", OK (ERecordExtend ("b", EVar "y", ERecordExtend ("a", EVar "x", ERecordEmpty))))
    ("{b = y ,a=x}", OK (ERecordExtend ("a", EVar "x", ERecordExtend ("b", EVar "y", ERecordEmpty))))
    // ("{a=x,h=w,d=y,b=q,g=z,c=t,e=s,f=r}",
    //     OK (record [("a", [EVar "x"]); ("b", [EVar "q"]); ("c", [EVar "t"]); ("d", [EVar "y"]);
    // ("e", [EVar "s"]); ("f", [EVar "r"]); ("g", [EVar "z"]); ("h", [EVar "w"])] ERecordEmpty));
    ("{a = x|m}", OK (ERecordExtend ("a", EVar "x", (EVar "m"))))
    ("{a | m}", Fail);
    // ("{ a = x, b = y | m}", OK (record [("a", [EVar "x"]); ("b", [EVar "y"])] (EVar "m")));
    // ("{ a = x, b = y | {m - a} }",
    //     OK (record [("a", [EVar "x"]); ("b", [EVar "y"])] (ERecordRestrict(EVar "m", "a"))));
    ("{ b = y | m - a }", Fail);
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
