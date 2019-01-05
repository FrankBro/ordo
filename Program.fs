module Program

open System
open System.IO

open Compiler
open Error
open Eval
open Expr
open Infer
open ParserExpr
open ParserType
open Repl
open Util

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
            name, ParserExpr.readExpr input
        )
    let ordoTy, ordoVal = compileExprs exprs
    ()

let testEmitter expected input =
    let expr = ParserExpr.readExpr input
    let ty = Infer.infer Map.empty expr
    let emit = Emit.emitExpr expr
    File.WriteAllText("output.lua", emit)

[<EntryPoint>]
let main argv =
    // let input = "let (a: int) = 1 in a"
    // test input

    // let input = "forall a => (a -> a) -> a"
    // testType input

    // let inputs =
    //     [
    //         "lib.ordo", "1"
    //         "main.ordo", "let a = open \"lib.ordo\" in a"
    //     ]
    // testCompiler inputs

    // runRepl ()
    [
        "let btrue = true in"
        "let bfalse = false in"
        "let i = 10 in"
        "let f = 3.14 in"
        "let s = \"string\" in"
        "let add a b = a + b in"
        "let c = add i 5 in"
        "print c"
    ]
    |> String.concat "\n"
    |> testEmitter "10"

    0 // return an integer exit code
