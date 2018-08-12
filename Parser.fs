module Parser

open System 
open Expr

open FParsec.CharParsers
open FParsec.Primitives

type ParserState = unit
type Parser<'t> = Parser<'t, ParserState>

let (<!>) (p: Parser<_>) label : Parser<_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

let spaces : Parser<_> = spaces1
let comma : Parser<_> = pstring ","
let identifier : Parser<string> =
    let first = letter 
    let rest = many (letter <|> digit)
    pipe2 first rest (fun first rest ->
        let name = 
            first :: rest
            |> Array.ofList
            |> String
        name
    )

let parseExpr, parseExprImpl = createParserForwardedToRef ()

let parseBool : Parser<Value> =
    (stringReturn "true" (VBool true))
    <|> (stringReturn "false" (VBool false))

let parseInt : Parser<Value> =
    pint32 |>> VInt

let parseFloat : Parser<Value> =
    pfloat |>> VFloat

let parseFun : Parser<Value> =
    let a = sepBy identifier spaces
    pipe3 (pstring "fun") a parseExpr (fun _ args body ->
        VFun (args, body)
    )

let parseValue : Parser<Expr> =
    choice [
        parseBool
        parseInt
        parseFloat
        parseFun
    ]
    |>> EValue

let parseVar : Parser<Expr> = 
    identifier |>> EVar <!> "parseVar"

let parseCall : Parser<Expr> =
    let a = sepBy parseExpr comma
    let b = pipe3 (pchar '(') (attempt a) (pchar ')') (fun _ expr _ ->
        expr
    )
    pipe2 parseVar b (fun name args ->
        ECall (name, args)
    ) <!> "parseCall"

let parseLet : Parser<Expr> =
    let name = pipe2 (pstring "let") identifier (fun _ name -> name)
    let value = pipe2 (pchar '=') parseExpr (fun _ value -> value)
    let body = pipe2 (pstring "in") parseExpr (fun _ body -> body)
    pipe3 name value body (fun name value body ->
        ELet (name, value, body)
    )

do parseExprImpl :=
    choice [
        parseValue
        parseVar
        parseCall
        parseLet
    ] <!> "parseExpr"

let inline readOrThrow (parser: Parser<'a,_>) input : 'a =
    match run parser input with
    | ParserResult.Success (result, state, pos) -> result
    | ParserResult.Failure (se, e, state) -> failwith "Parser error" 
let inline readExpr input = readOrThrow parseExpr input
let inline readExprList input : Expr list =
    let parser = (sepEndBy parseExpr spaces)
    readOrThrow parser input
