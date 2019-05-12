module Parse

open FSharp.Text.Lexing

let parse input =
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
        printf "Parse failed at line %d, column %d:\n" line column
        printf "Last loken: %s" lastToken
        printf "\n"
        System.Console.ReadLine() |> ignore
        exit 1
