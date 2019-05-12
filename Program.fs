module Program

open System
open System.Diagnostics
open System.IO

open Compiler
open Error
open Eval
open Expr
open Infer
open Parse
open Repl
open Util
open TestEmitter

let test input =
    Infer.resetId ()
    try
        printfn "Input    : %s" input
        let expr = parse input
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

// let testType input =
//     try
//         printfn "Input    : %s" input
//         let ty = parse input
//         printfn "Type     : %O" ty
//         printfn "Raw type : %s" (stringOfTy ty)
//     with 
//     | OrdoException e ->
//         printfn "Exception"
//         printfn "%O" e
//     | e ->
//         printfn "Exception"
//         printfn "%O" e

let testCompiler files =
    let exprs =
        files
        |> List.map (fun (name, input) ->
            name, parse input
        )
    let ordoTy, ordoVal = compileExprs exprs
    ()

let testEmitter expected input =
    try
        let expr = parse input
        let ty = Infer.infer Map.empty expr
        printfn "%s" (stringOfTy ty)
        let transformed = Transform.transform expr
        printfn "%s" (stringOfExpr transformed)
        let emit = Emit.emit transformed
        File.WriteAllText("output.lua", emit)
    with 
        | OrdoException e ->
            printfn "OrdoException: %O" e
        | e ->
            printfn "Exception: %O" e
    // let p = new Process()
    // p.StartInfo.UseShellExecute <- false
    // p.StartInfo.RedirectStandardOutput <- true
    // p.StartInfo.FileName <- "lua/lua53.exe output.lua"
    // p.StartInfo.FileName <- "PowerShell.exe"
    // p.StartInfo.Arguments <- "/Command \"lua\\lua53.exe output.lua\""
    // p.Start() |> ignore<bool>
    // let output = p.StandardOutput.ReadToEnd()
    // p.WaitForExit()
    // printfn "Output = %s" output
    ()

let testTransform input transform =
    let parsed = parse input
    try
        let transformed = Transform.transform parsed
        let parsedTransform = parse transform
        if transformed <> parsedTransform then
            printfn "%s" (stringOfExpr transformed)
            printfn "%s" (stringOfExpr parsedTransform)
    with 
        | OrdoException e ->
            printfn "%O" e
        | e ->
            printfn "%O" e

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

    // testTransform
    //     "let (:a a) = (:a 1) in a"
    //     "let _var0 = (:a 1) in match _var0 { :a a -> a }"

    // [
    //     "let add a b = a + b in "
    //     "let add_one = add 1 in "
    //     "let value = "
    //     "    match { a = 1 } { "
    //     "        { a = 1 } -> 1 "
    //     "    } in "
    //     "print(add_one value)"
    // ]
    // |> String.concat "\n"
    // |> testEmitter "10"

    File.ReadAllLines "bootstrap/main.ordo"
    |> String.concat "\n"
    |> testEmitter ""

    0 // return an integer exit code
