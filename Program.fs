module Program

open System
open System.Diagnostics
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
    let emit = Emit.emit expr
    File.WriteAllText("output.lua", emit)
    let p = new Process()
    p.StartInfo.UseShellExecute <- false
    p.StartInfo.RedirectStandardOutput <- true
    // p.StartInfo.FileName <- "lua/lua53.exe output.lua"
    p.StartInfo.FileName <- "PowerShell.exe"
    p.StartInfo.Arguments <- "/Command \"lua\\lua53.exe output.lua\""
    p.Start() |> ignore<bool>
    let output = p.StandardOutput.ReadToEnd()
    p.WaitForExit()
    printfn "Output = %s" output
    ()

let testTransform parse transform =
    let parsed = ParserExpr.readExpr parse
    try
        let transformed = Transform.transform parsed
        let parsedTransform = ParserExpr.readExpr transform
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
    // testTransform
    //     "let { a = { b = b } } = { a = { b = 1 } } in b"
    //     "let _var0 = { a = { b = 1 } } in let _var1 = _var0.a in let b = _var1.b in b"
    //     "let _var0 = { a = (:b 1) } in match _var0.a { :b b -> b }"

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
        "let yfact fact n ="
        "    if n > 0 then"
        "        n * fact(n-1)"
        "    else"
        "        1 "
        "in"
        "let fact = fix yfact in"
        "print (fact 5)"
    ]
    |> String.concat "\n"
    |> testEmitter "10"

    0 // return an integer exit code
