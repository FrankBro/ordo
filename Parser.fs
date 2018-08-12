module Parser

open System 

open FParsec.CharParsers
open FParsec.Primitives

open Expr
open Util 

type ParserState = unit
type Parser<'t> = Parser<'t, ParserState>

let ws = spaces
let str s = pstring s

let (<!>) (p: Parser<_>) label : Parser<_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

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
let parseSimpleExpr, parseSimpleExprImpl = createParserForwardedToRef ()

let parseBool : Parser<Value> =
    (stringReturn "true" (VBool true))
    <|> (stringReturn "false" (VBool false))

let parseInt : Parser<Value> =
    pint32 |>> VInt

let parseFloat : Parser<Value> =
    pfloat |>> VFloat

// let parseFun : Parser<Value> =
//     let idents = sepBy identifier spaces1
//     pipe3 (str "fun") idents parseExpr (fun _ args body ->
//         VFun (args, body)
//     )

let parseValue : Parser<Expr> =
    choice [
        parseBool
        parseInt
        parseFloat
        // parseFun
    ]
    |>> EValue

let parseVar : Parser<Expr> = 
    identifier |>> EVar <!> "parseVar"

let parseIdentList, parseIdentListImpl = createParserForwardedToRef ()
do parseIdentListImpl :=
    choice [
        identifier |>> 
            (fun ident -> [ident])
        pipe2 identifier (ws >>. parseIdentList)
            (fun ident idents -> ident :: idents)
    ]

let parseExprList, parseExprListImpl = createParserForwardedToRef ()
do parseExprListImpl :=
    choice [
        parseExpr |>> 
            (fun expr -> [expr])
        pipe2 parseExpr (ws >>. parseExprList)
            (fun expr exprs -> expr :: exprs)
    ]

let parseCall : Parser<Expr> =
    choice [
        pipe2 parseSimpleExpr (between (ws >>. str "(") (ws >>. str ")") parseExprList)
            (fun expr exprs -> ECall (expr, exprs))
        pipe3 (ws >>. str "(") parseSimpleExpr (ws >>. str ")")
            (fun _ expr _ -> ECall (expr, []))
    ]

// let parseLet : Parser<Expr> =
//     let name = pipe2 (str "let") identifier (fun _ name -> name)
//     let value = pipe2 (pchar '=') parseExpr (fun _ value -> value)
//     let body = pipe2 (str "in") parseExpr (fun _ body -> body)
//     pipe3 name value body (fun name value body ->
//         ELet (name, value, body)
//     )

// Yeah most designs they'll have expressions and atoms.

do parseExprImpl :=
    choice [
        parseSimpleExpr
        // parseLet
        // parseFun
    ] <!> "parseExpr"

do parseSimpleExprImpl :=
    choice [
        // parseValue
        parseVar
        parseCall
    ]

let inline readOrThrow (parser: Parser<'a,_>) input : 'a =
    match run parser input with
    | ParserResult.Success (result, state, pos) -> result
    | ParserResult.Failure (se, e, state) -> failwith "Parser error" 
let inline readExpr input = readOrThrow parseExpr input
let inline readExprList input : Expr list =
    let parser = (sepEndBy parseExpr ws)
    readOrThrow parser input
