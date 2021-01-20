module Program

open System
open System.Diagnostics
open System.IO

open Error
open Expr
open Infer
open Parse
open Repl
open Util

[<EntryPoint>]
let main argv =
    runRepl ()
    0 // return an integer exit code
