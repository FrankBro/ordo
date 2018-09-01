module Program

open System

open Error
open Eval
open Expr
open Infer
open Parser

let test input =
    try
        printfn "Input"
        printfn "%s" input
        let expr = readExpr input
        printfn "Expr"
        printfn "%O" expr
        printfn "Raw expr"
        printfn "%s" (stringOfExpr expr)
        printfn "Type"
        let ty = infer expr
        printfn "%O" ty
        printfn "Raw type"
        printfn "%s" (stringOfTy ty)
        printfn "Value"
        let value = eval expr
        printfn "%O" value
        printfn "Raw value"
        printfn "%s" (stringOfValue value)
    with 
    | OrdoException e ->
        printfn "Exception"
        printfn "%O" e
    | e ->
        printfn "Exception"
        printfn "%O" e

[<EntryPoint>]
let main argv =
    // let input = "match :x 1 { :x i -> i | :y i -> i }"
    let input = "let { b = b, a = a | r } = { a = 1, c = 2, b = 3 } in a"
    test input
    0 // return an integer exit code
