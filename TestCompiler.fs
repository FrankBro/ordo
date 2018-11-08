module TestCompiler

open Xunit

open Compiler
open Error
open Expr
open TestUtil

let testCompiler files (inferExpected: InferResult) (evalExpected: EvalResult) =
    let exprs =
        files
        |> List.map (fun (name, input) ->
            name, Parser.readExpr input
        )
    let ordoTy, ordoVal = compileExprs exprs
    if inferExpected <> ISkip then
        Assert.StrictEqual(inferExpected, IOk (stringOfTy ordoTy))
    if evalExpected <> ESkip then 
        Assert.StrictEqual(evalExpected, EOk ordoVal)


[<Fact>]
let ``Simple two files`` () =
    testCompiler
        [
            "lib.ordo", "1"
            "main.ordo", "let a = open \"lib.ordo\" in a"
        ]
        (IOk "int")
        (EOk (VInt 1))
