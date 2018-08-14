module Program

open System

open Eval
open Expr
open Infer
open Parser

[<EntryPoint>]
let main argv =
    let input = "let a = 1 in a"
    let expr = readExpr input
    let ty = infer expr
    let value = eval expr
    printfn "Input"
    printfn "%s" input
    printfn "Expr"
    printfn "%O" expr
    printfn "Raw expr"
    printfn "%s" (Expr.ToStringRaw expr)
    printfn "Type"
    printfn "%O" ty
    printfn "Raw type"
    printfn "%s" (Ty.ToStringRaw ty)
    printfn "Value"
    printfn "%O" value
    printfn "Raw value"
    printfn "%s" (Value.ToStringRaw value)

    0 // return an integer exit code
