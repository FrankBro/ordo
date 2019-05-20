module Compiler

open System.IO

open Error
open Expr
open Infer
open Parse

let compile (folder: string) (files: string list) =
    Infer.resetId ()
    let extract state (name: string, expr: Expr) =
        let ordoTy =
            Infer.infer state expr
            |> generalize
        let ordoVal = Emit.emit expr
        ordoTy, ordoVal
    let rec loop (types: Map<string, Ty>) files =
        match files with
        | [] -> ()
        | file :: files ->
            let inputFile = sprintf "%s/%s.ordo" folder file
            let text =
                File.ReadAllLines(inputFile)
                |> String.concat "\n"
            let expr = parse file text
            let ty =
                Infer.infer types expr
                |> generalize
            let output = Emit.emit expr
            let outputFile = sprintf "output/%s.lua" file
            File.WriteAllText(outputFile, output)
            let types = Map.add file ty types
            loop types files
    Directory.Delete("output", true)
    Directory.CreateDirectory("output") |> ignore
    File.Copy("std.lua", "output/std.lua")
    loop Map.empty files
