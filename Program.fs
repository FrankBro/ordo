module Program

open System
open System.Diagnostics
open System.IO

open Compiler
open Error
open Expr
open Infer
open Parse
open Repl
open Util
open TestEmitter

[<EntryPoint>]
let main argv =
    [
        "ffi"
        "expr"
    ]
    |> compile "bootstrap"

    System.Console.ReadKey () |> ignore

    0 // return an integer exit code
