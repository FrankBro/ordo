module TestEmitter

open System
open System.IO

open Xunit
open Xunit.Abstractions

open Parse

let testEmitter expected input =
    let expr = parse "test" input
    let ty = Infer.infer Map.empty expr
    let emit = Emit.emit expr
    File.WriteAllText("output.lua", emit)

// [<Fact>]
// let ``Test emit`` () =
//     [
//         "let btrue = true in"
//         "let bfalse = false in"
//         "let i = 10 in"
//         "let f = 3.14 in"
//         "let s = \"string\" in"
//         "i"
//     ]
//     |> String.concat "\n"
//     |> testEmitter "10"
