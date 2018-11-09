module Program

open System

open Compiler
open Error
open Eval
open Expr
open Infer
open Parser
open Repl

let test input =
    Infer.resetId ()
    try
        printfn "Input    : %s" input
        let expr = readExpr input
        printfn "Expr     : %O" expr
        printfn "Raw expr : %s" (stringOfExpr expr)
        let ty = infer Map.empty expr
        printfn "Type     : %O" ty
        printfn "Raw type : %s" (stringOfTy ty)
        let value = eval Map.empty expr
        printfn "Value    : %O" value
        printfn "Raw value: %s" (stringOfValue value)
    with 
    | OrdoException e ->
        printfn "Exception"
        printfn "%O" e
    | e ->
        printfn "Exception"
        printfn "%O" e

let testType input =
    try
        printfn "Input    : %s" input
        let ty = readType input
        printfn "Type     : %O" ty
        printfn "Raw type : %s" (stringOfTy ty)
    with 
    | OrdoException e ->
        printfn "Exception"
        printfn "%O" e
    | e ->
        printfn "Exception"
        printfn "%O" e

let testCompiler files =
    let exprs =
        files
        |> List.map (fun (name, input) ->
            name, Parser.readExpr input
        )
    let ordoTy, ordoVal = compileExprs exprs
    ()


[<EntryPoint>]
let main argv =
    // let input = "match { a = 1 } { { a = 2 } -> 2 | otherwise -> 1 }"
    // test input

    // let input = "forall a => (a -> a) -> a"
    // testType input

    let inputs =
        [
            "lib.ordo", "1"
            "main.ordo", "let a = open \"lib.ordo\" in a"
        ]
    testCompiler inputs

    // runRepl ()

    0 // return an integer exit code
