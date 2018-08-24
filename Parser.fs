module Parser

open System 

open FParsec.CharParsers
open FParsec.Primitives

open Expr
open Infer
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

let reserved = [ "let"; "in"; "fun"; "match" ]

let identifier : Parser<string> =
    many1 lower |>> (Array.ofList >> String)
    >>= fun s ->
        if reserved |> List.exists ((=) s) then 
            fail "reserved"
        else
            preturn s

let parseBool : Parser<Expr> =
    (stringReturn "true" (EBool true))
    <|> (stringReturn "false" (EBool false))

let parseInt : Parser<Expr> =    
    let pa = opt (str "-")
    let pb = many1 digit |>> (Array.ofList >> String)
    pipe2 pa pb (fun sign whole ->
        let number = whole |> int
        if Option.isSome sign then
            -number
        else
            number
    )
    |>> EInt

let parseFloat : Parser<Expr> =
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
    |>> EFloat

let parseFun : Parser<Expr> =
    let p1 = (str "fun" >>. ws1) >>. identifier .>> ws
    let p2 = (str "->" >>. ws) >>. parseExpr .>> ws
    (p1 .>>. p2) |>> EFun

let parseVar = 
    identifier |>> EVar

let parseParen = 
    between (str "(" >>. ws) (str ")" >>. ws) (parseExpr .>> ws)

let parseLet = 
    let p1 = (str "let" >>. ws1) >>. identifier .>> ws
    let p2 = (str "=" >>. ws) >>. parseExpr .>> ws 
    let p3 = (str "in" >>. ws1) >>. parseExpr .>> ws
    pipe3 p1 p2 p3 (fun var value body -> ELet (var, value, body))

let parseVariant =
    (str ":" .>> ws) >>. (identifier .>> ws) .>>. (parseExpr .>> ws) 
    |>> EVariant

let parseMatchNormalCase =
    let pa = str ":" >>. identifier .>> ws
    let pb = identifier .>> ws 
    let pc = str "->" .>> ws >>. parseExpr
    pipe3 pa pb pc (fun label var expr -> (Some label, var, expr))

let parseMatchDefaultCase =
    let pa = identifier .>> ws
    let pb = str "->" .>> ws >>. parseExpr
    pipe2 pa pb (fun var expr -> (None, var, expr))

let parseMatchCase : Parser<string option * string * Expr> =
    parseMatchNormalCase
    <|> parseMatchDefaultCase

let parseMatchCases =
    attempt (sepBy (parseMatchCase .>> ws) (str "|" .>> ws))
    <|> ((parseMatchCase .>> ws) |>> List.singleton)

let parseMatch =
    let p1 = (str "match" .>> ws) >>. (parseExpr .>> ws)
    let p2 = between (str "{" .>> ws) (str "}" .>> ws) parseMatchCases
    pipe2 p1 p2 (fun expr cases  -> 
        let normals =
            cases 
            |> List.choose (fun (oLabel, var, expr) -> 
                oLabel
                |> Option.map (fun label -> label, var, expr)
            )
        let oDefault =
            cases
            |> List.tryPick (fun (oLabel, var, expr) ->
                match oLabel with
                | None -> Some (var, expr)
                | Some _ -> None
            )
        ECase(expr, normals, oDefault)
    )

let parseRecordEmpty : Parser<Expr> = 
    (str "{" .>> ws) .>> (str "}" .>> ws) 
    |>> fun _ -> ERecordEmpty

let exprRecordExtend labelExprList record =
    (record, labelExprList)
    ||> List.fold (fun record (label, expr) -> ERecordExtend (label, expr, record))

let parseRecordLabel : Parser<string * Expr> =
    (identifier .>> ws .>> str "=" .>> ws) .>>. (parseExpr .>> ws)

let parseRecordLabels : Parser<(string * Expr) list> =
    attempt (sepBy (parseRecordLabel .>> ws) (str "," .>> ws))
    <|> ((parseRecordLabel .>> ws) |>> List.singleton)

let parseRecordExtend =
    let p1 = (str "{" .>> ws) >>. (parseRecordLabels .>> ws)
    let p2 = (str "|" .>> ws) >>. (parseExpr .>> ws) .>> (str "}" .>> ws)
    pipe2 p1 p2 exprRecordExtend

let parseRecordInit =
    (str "{" .>> ws) >>. (parseRecordLabels .>> ws) .>> (str "}" .>> ws)
    |>> fun x -> exprRecordExtend x ERecordEmpty

let parseRecordRestrict =
    let p1 = (str "{" .>> ws) >>. (parseExpr .>> ws)
    let p2 = (str "-" .>> ws) >>. (identifier .>> ws) .>> (str "}" .>> ws)
    pipe2 p1 p2 (fun expr label -> ERecordRestrict (expr, label))

let parseRecordSelect =
    (parseExpr .>> ws) .>>. (str "." >>. ws >>. identifier)
    |>> ERecordSelect

let parseNotCallOrRecordSelect =
    choice [
        parseParen
        parseBool
        attempt parseFloat
        attempt parseInt 
        parseFun 
        parseLet
        attempt parseVar
        parseVariant
        parseMatch
        attempt parseRecordEmpty
        attempt parseRecordExtend
        attempt parseRecordInit
        attempt parseRecordRestrict
    ]

let parseAnything  =
    many1 (parseNotCallOrRecordSelect .>> ws)
    >>= fun result ->
        match result with
        | [one] ->
            attempt (str "." >>. ws >>. identifier) |>> fun field -> ERecordSelect (one, field)
            <|> preturn one
        | _ -> 
            let rec loop state exprs =
                match state, exprs with
                | None, fn :: arg :: exprs -> loop (Some (ECall (fn, arg))) exprs
                | None, _ -> failwith "should never happen"
                | Some fn, arg :: exprs -> loop (Some (ECall (fn, arg))) exprs
                | Some expr, [] -> expr
            let calls = loop None result
            preturn calls

do parseExprRef := parseAnything

let inline readOrThrow (parser: Parser<'a,ParserState>) input : 'a =
    match runParserOnString parser ParserState.New "" input with
    | ParserResult.Success (result, state, pos) -> result
    | ParserResult.Failure (se, e, state) -> 
        failwith "Parser error" 
let inline readExpr input = readOrThrow parseExpr input
let inline readExprList input : Expr list =
    let parser = (sepEndBy parseExpr ws)
    readOrThrow parser input

let parseTy, parseTyRef = createParserForwardedToRef ()

let identifierWs = identifier .>> ws
let parseTyWs = parseTy .>> ws
let strWs s = str s .>> ws
let parseTyListWs = sepBy parseTyWs (strWs ",")

let parseTConst =
    identifierWs |>> TConst

let parseTApp : Parser<Ty> =
    parseTyWs .>> strWs "[" .>>. parseTyListWs .>> strWs "]"
    |>> TApp

let parseTArrow : Parser<Ty> =
    parseTyWs .>> strWs "->" .>>. parseTyWs
    |>> TArrow

let nameToIdMap = ref Map.empty

let nameToId s : Ty =
    let m = !nameToIdMap
    m 
    |> Map.tryFind s 
    |> Option.defaultWith (fun () ->
        let id = newGenVar ()
        nameToIdMap := Map.add s id m
        id
    )

let parseTVar : Parser<Ty> =
    strWs "'" >>. identifierWs
    |>> nameToId

let parseTEmptyRecord : Parser<Ty> =
    strWs "{" .>> strWs "}"
    |>> fun _ -> TRecord TRowEmpty

let parseTRowFields : Parser<(string * Ty) list> =
    let field = identifierWs .>>. parseTyWs
    sepBy1 field (strWs ",")

// let parseTRecord : Parser<Ty> =
//     strWs "{" >>. parseTRowFields .>> strWs "}"
//     |>> fun fields ->
//         let fixedFields =
//             fields
//             |> Map.ofList
//             |> Map.map (fun _ -> List.singleton)
//         TRowExtend (fixedFields, TRowEmpty)

// let parseTExtendRecord : Parser<Ty> =
//     strWs "{" >>. parseTRowFields .>> strWs "|" .>>. parseTyWs .>> strWs "}"
//     |>> fun (fields, record) ->
//         let fixedFields =
//             fields
//             |> Map.ofList
//             |> Map.map (fun _ -> List.singleton)
//         TRowExtend (fixedFields, record)
