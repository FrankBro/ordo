module Parse

open System
open System.Text

open FSharp.Text.Lexing

let parse (file: string) (input: string) =
    let tokenized = LexBuffer<char>.FromString(input)
    try
        Parser.start Lexer.tokenize tokenized
        |> Option.get
    with e ->
        let pos = tokenized.EndPos
        let line = pos.Line
        let column = pos.Column
        let message = e.Message
        let lastToken = new System.String(tokenized.Lexeme)
        printf "Message: %s\n" message
        printf "File: %s\n" file
        printf "Parse failed at line %d, column %d:\n" line column
        printf "Last loken: %s\n" lastToken
        printf "Input:\n%s\n" input
        let split = input.Split('\n')
        let rec loop (line, column) (xs: string list) =
            match xs with
            | [] -> line - 1, column + 1
            | x :: xs ->
                let len = x.Length
                let diff = column - len
                if column < len then
                    line, column + 1
                else
                    loop (line + 1, diff - 1) xs
        let line, column =
            split
            |> List.ofArray
            |> loop (1, column)
        printf "Line: %d, Column: %d" line column
        System.Console.ReadLine() |> ignore
        exit 1
