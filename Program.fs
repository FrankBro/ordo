module Program

open System

open Error
open Eval
open Expr
open Infer
open Parser
open Repl

let test input =
    try
        printfn "Input    : %s" input
        let expr = readExpr input
        printfn "Expr     : %O" expr
        printfn "Raw expr : %s" (stringOfExpr expr)
        let ty = infer expr
        printfn "Type     : %O" ty
        printfn "Raw type : %s" (stringOfTy ty)
        let value = eval expr
        printfn "Value    : %O" value
        printfn "Raw value: %s" (stringOfValue value)
    with 
    | OrdoException e ->
        printfn "Exception"
        printfn "%O" e
    | e ->
        printfn "Exception"
        printfn "%O" e

[<EntryPoint>]
let main argv =
    let input = 
        "let f {x,y} = x + y in 
         let x = 1 in 
         let y = 2 in 
         f {x,y}"
    test input
    // runRepl ()
    0 // return an integer exit code
