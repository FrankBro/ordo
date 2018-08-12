module Parser

open System 

open FParsec.CharParsers
open FParsec.Primitives

open Expr
open Util 

type ParserState = unit
type Parser<'t> = Parser<'t, ParserState>

let (<!>) (p: Parser<_>) label : Parser<_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

let ws = spaces
let str s = pstring s

let parseExpr, parseExprRef = createParserForwardedToRef ()

let identifier : Parser<string> =
    many1 lower |>> (Array.ofList >> String)

let parseVar = identifier |>> EVar

let parseFun : Parser<Expr> =
    let p1 = str "fun" >>. identifier
    let p2 = str "->" >>. parseExpr
    (p1 .>>. p2) |>> EFun

let parseParen = between (ws >>. str "(") (ws >>. str ")") parseExpr

let parseLet = 
    let p1 = str "let" >>. identifier
    let p2 = str "=" >>. parseExpr
    let p3 = str "in" >>. parseExpr
    pipe3 p1 p2 p2 (fun var value body -> ELet (var, value, body))

let parseCall =
    let parseNotCall = parseParen <|> parseFun <|> parseLet <|> parseVar
    chainl1 parseNotCall (pchar ' ' |>> (fun _ f a -> ECall(f, a)))

do parseExprRef := parseCall

let inline readOrThrow (parser: Parser<'a,_>) input : 'a =
    match run parser input with
    | ParserResult.Success (result, state, pos) -> result
    | ParserResult.Failure (se, e, state) -> failwith "Parser error" 
let inline readExpr input = readOrThrow parseExpr input
let inline readExprList input : Expr list =
    let parser = (sepEndBy parseExpr ws)
    readOrThrow parser input
