module Program

open System

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

[<EntryPoint>]
let main argv =
    let input = "match { a = 1 } { { a = 2 } -> 2 | otherwise -> 1 }"
    test input

    // let input = "forall a => (a -> a) -> a"
    // testType input

    // runRepl ()

    0 // return an integer exit code
