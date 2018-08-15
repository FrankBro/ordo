module Parser

open System 

open FParsec.CharParsers
open FParsec.Primitives

open Expr
open Util 

type LetState =
    | NotInLet
    | LetDone
    | EqualDone
    | InDone

type ParserState = {
    LetState: LetState
}
with
    static member New = {
        LetState = NotInLet
    }

type Parser<'t> = Parser<'t, ParserState>

let (<!>) (p: Parser<_>) label : Parser<_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

let ws = spaces
let ws1 = spaces1
let str s = pstring s

let parseExpr, parseExprRef = createParserForwardedToRef ()

let reserved = [ "let"; "in"; "fun" ]

let identifier : Parser<string> =
    many1 lower |>> (Array.ofList >> String)
    >>= fun s ->
        if reserved |> List.exists ((=) s) then 
            fail "reserved"
        else
            preturn s

let parseBool : Parser<Value> =
    (stringReturn "true" (VBool true))
    <|> (stringReturn "false" (VBool false))

let parseInt : Parser<Value> =    
    let pa = opt (str "-")
    let pb = many1 digit |>> (Array.ofList >> String)
    pipe2 pa pb (fun sign whole ->
        let number = whole |> int
        if Option.isSome sign then
            -number
        else
            number
    )
    |>> VInt

let parseFloat : Parser<Value> =
    let pa = opt (str "-")
    let pb = many1 digit |>> (Array.ofList >> String)
    let pc = str "." >>. many digit |>> (Array.ofList >> String)
    pipe3 pa pb pc (fun sign whole decimal -> 
        let number = whole + "." + decimal |> float
        if Option.isSome sign then
            -number
        else
            number
    ) 
    |>> VFloat

let parseFun : Parser<Value> =
    let p1 = (str "fun" >>. ws1) >>. identifier .>> ws
    let p2 = (str "->" >>. ws) >>. parseExpr .>> ws
    (p1 .>>. p2) |>> VFun

let parseValue : Parser<Expr> =
    choice [
        parseBool
        attempt parseFloat
        parseInt 
        parseFun 
    ]
    |>> EValue

let parseVar = 
    identifier |>> EVar

let parseParen = 
    between (str "(" >>. ws) (str ")" >>. ws) (parseExpr .>> ws)

let parseLet = 
    let p1 = (str "let" >>. ws1) >>. identifier .>> ws
    let p2 = (str "=" >>. ws) >>. parseExpr .>> ws 
    let p3 = (str "in" >>. ws1) >>. parseExpr .>> ws
    pipe3 p1 p2 p3 (fun var value body -> ELet (var, value, body))

let parseNotCall =
    choice [
        parseParen
        parseValue
        parseLet
        attempt parseVar
    ]

let parseSingleOrCall  =
    many1 (parseNotCall .>> ws)
    >>= fun result ->
        match result with
        | [one] -> preturn one
        | _ -> 
            let rec loop state exprs =
                match state, exprs with
                | None, fn :: arg :: exprs -> loop (Some (ECall (fn, arg))) exprs
                | None, _ -> failwith "should never happen"
                | Some fn, arg :: exprs -> loop (Some (ECall (fn, arg))) exprs
                | Some expr, [] -> expr
            let calls = loop None result
            preturn calls

do parseExprRef := parseSingleOrCall

let inline readOrThrow (parser: Parser<'a,ParserState>) input : 'a =
    match runParserOnString parser ParserState.New "" input with
    | ParserResult.Success (result, state, pos) -> result
    | ParserResult.Failure (se, e, state) -> 
        failwith "Parser error" 
let inline readExpr input = readOrThrow parseExpr input
let inline readExprList input : Expr list =
    let parser = (sepEndBy parseExpr ws)
    readOrThrow parser input
