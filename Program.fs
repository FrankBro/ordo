module Program

open System

open Eval
open Expr
open Infer
open Parser

[<EntryPoint>]
let main argv =
    try
        let input = "let a = 1 in a"
        printfn "Input"
        printfn "%s" input
        let expr = readExpr input
        printfn "Expr"
        printfn "%O" expr
        printfn "Raw expr"
        printfn "%s" (Expr.ToStringRaw expr)
        printfn "Type"
        let ty = infer expr
        printfn "%O" ty
        printfn "Raw type"
        printfn "%s" (Ty.ToStringRaw ty)
        printfn "Value"
        let value = eval expr
        printfn "%O" value
        printfn "Raw value"
        printfn "%s" (Value.ToStringRaw value)
    with e ->
        printfn "Exception"
        printfn "%O" e

    0 // return an integer exit code
