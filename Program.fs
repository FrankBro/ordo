﻿module Program

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
    // "match :a { a = 1 } { :a { a = a } -> a }"
    // "match :a (:b 2) { :a (:b b) -> b }"
    // "match :a 1 { :a a -> 1 , :y a -> 2 }"
    // "match :b 1 { :a a -> 1 | otherwise -> 2 }"
    let input = "match :a { a = 1 } { :a { a = a } -> a }"
    test input
    // runRepl ()
    0 // return an integer exit code
