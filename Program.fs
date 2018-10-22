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
    // let input = "{ x = 1 | { x = 0 }\\x }"
    // test input
    // runRepl ()
    let print i =
        printfn "%s, %O" i (readType i)
    // print "bool"
    // print "int"
    // print "float"
    // print "const"
    // print "{}"
    // print "{ x : int, y : int }"
    // print "{ x : int | r }"
    // print "< x : int, y : int >"
    // print "< x : int | r >"
    // print "int -> int -> int"
    // print "list[int]"
    print "forall a => a -> a -> a"
    0 // return an integer exit code
