module Parser

open System 

open FParsec
open FParsec.CharParsers
open FParsec.Primitives

open Error
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
let strWs s = str s .>> ws
let strWs1 s = str s .>> ws1

let parseExpr, parseExprRef = createParserForwardedToRef ()
let parseExprWs = parseExpr .>> ws

let parsePattern, parsePatternRef = createParserForwardedToRef ()
let parsePatternWs = parsePattern .>> ws

let reserved = [ "let"; "in"; "fun"; "match"; "if"; "then"; "else"; "true"; "false" ]

let ident: Parser<string> =
    many1 lower |>> (Array.ofList >> String)
    >>= fun s ->
        if reserved |> List.exists ((=) s) then 
            fail "reserved"
        else
            preturn s
let identWs = ident .>> ws

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
    let p1 = strWs1 "fun" >>. many1 parsePatternWs
    let p2 = strWs "->" >>. parseExprWs
    pipe2 p1 p2 (fun patterns body ->
        (patterns, body)
        ||> List.foldBack (fun pattern state ->
            EFun (pattern, state)
        )
    )

let parseVar = 
    identWs |>> EVar

let parseParen element = 
    between (strWs "(") (strWs ")") element

let parseLet = 
    let p1 = strWs1 "let" >>. many1 parsePatternWs
    let p2 = strWs "=" >>. parseExprWs
    let p3 = opt (strWs1 "in" >>. parseExprWs)
    pipe3 p1 p2 p3 (fun patterns value oBody -> 
        let pattern, value =
            match patterns with
            | [pattern] -> pattern, value
            | EVar name :: rest ->
                let wrappedValue =
                    (rest, value)
                    ||> List.foldBack (fun pattern state ->
                        EFun (pattern, state)
                    )
                EVar name, wrappedValue
            | _ -> raise (parserError InvalidFunctionDeclaration)
        let body =
            oBody
            |> Option.defaultWith (fun () ->
                match patterns with
                | [] -> raise (parserError InvalidFunctionDeclaration)
                | pattern :: _ -> pattern
            )
        ELet (pattern, value, body))

let parseVariant =
    strWs ":" >>. identWs .>>. parseExprWs
    |>> EVariant

let parseMatchNormalCase : Parser<Pattern * Expr> =
    let pa = parsePatternWs
    let pb = strWs "->" >>. parseExprWs
    pipe2 pa pb (fun pattern expr -> (pattern, expr))

let parseMatchDefaultCase : Parser<string * Expr> =
    let pa = identWs
    let pb = strWs "->" >>. parseExprWs
    pipe2 pa pb (fun var expr -> (var, expr))

// let parseMatchCase : Parser<string option * Pattern * Expr> =
//     parseMatchNormalCase 
//     <|> parseMatchDefaultCase

// let parseMatchCases =
//     attempt (sepBy parseMatchCase (strWs "|"))
//     <|> (parseMatchCase |>> List.singleton)

let parseMatch =
    let p1 = strWs "match" >>. parseExprWs
    let p2 = strWs "{" >>. (sepBy parseMatchNormalCase (strWs ",")) 
    let p3 = opt (strWs "|" >>. parseMatchDefaultCase) .>> strWs "}"
    pipe3 p1 p2 p3 (fun expr normals oDefault -> 
        ECase(expr, normals, oDefault)
    )

let parseRecordEmpty : Parser<Expr> = 
    strWs "{" .>> strWs "}"
    |>> fun _ -> ERecordEmpty

let exprRecordExtend labelExprList record =
    (record, labelExprList)
    ||> List.fold (fun record (label, expr) -> ERecordExtend (label, expr, record))

let parseRecordLabel content : Parser<string * Expr> =
    identWs .>> strWs "=" .>>. content

let parseRecordLabels content : Parser<(string * Expr) list> =
    attempt (sepBy (parseRecordLabel content) (strWs ","))
    <|> (parseRecordLabel content|>> List.singleton)

let parseRecordExtend content =
    let p1 = strWs "{" >>. (parseRecordLabels content .>> ws)
    let p2 = strWs "|" >>. parseExprWs .>> strWs "}"
    pipe2 p1 p2 exprRecordExtend

let parseRecordInit content =
    strWs "{" >>. (parseRecordLabels content .>> ws) .>> strWs "}"
    |>> fun x -> exprRecordExtend x ERecordEmpty

let parseRecordSelect =
    parseExprWs .>>. (strWs "." >>. identWs)
    |>> ERecordSelect

let parseIfThenElse =
    let p1 = strWs1 "if" >>. parseExprWs
    let p2 = strWs1 "then" >>. parseExprWs
    let p3 = strWs1 "else" >>. parseExprWs
    pipe3 p1 p2 p3 (fun ifExpr thenExpr elseExpr -> EIfThenElse (ifExpr, thenExpr, elseExpr))

do parsePatternRef :=
    choice [
        parseParen parsePatternWs
        parseVariant
        attempt parseVar
        attempt parseRecordEmpty
        attempt (parseRecordExtend parsePatternWs)
        attempt (parseRecordInit parsePatternWs)
    ]

// let opp = OperatorPrecedenceParser<Expr, unit, ParserState>()
// opp.TermParser <- choice [
//     attempt parseFloat
//     attempt parseInt
//     attempt parseVar
// ]

// opp.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, (fun a b -> EBinOp (a, Plus, b))))

let parseNotCallOrRecordSelect =
    choice [
        parseParen parseExprWs
        parseBool
        attempt parseFloat
        attempt parseInt 
        parseFun 
        parseLet
        attempt parseVar
        parseVariant
        parseMatch
        attempt parseRecordEmpty
        attempt (parseRecordExtend parseExprWs)
        attempt (parseRecordInit parseExprWs)
        attempt parseIfThenElse
    ]

let parseAnything  =
    many1 (parseNotCallOrRecordSelect .>> ws)
    >>= fun result ->
        match result with
        | [one] ->
            choice [
                attempt (strWs "+" >>. parseExprWs) |>> fun two -> EBinOp (one, Plus, two)
                attempt (strWs "-" >>. parseExprWs) |>> fun two -> EBinOp (one, Minus, two)
                attempt (strWs "*" >>. parseExprWs) |>> fun two -> EBinOp (one, Multiply, two)
                attempt (strWs "/" >>. parseExprWs) |>> fun two -> EBinOp (one, Divide, two)
                attempt (strWs "&&" >>. parseExprWs) |>> fun two -> EBinOp (one, And, two)
                attempt (strWs "||" >>. parseExprWs) |>> fun two -> EBinOp (one, Or, two)
                attempt (strWs "=" >>. parseExprWs) |>> fun two -> EBinOp (one, Equal, two)
                attempt (strWs "<>" >>. parseExprWs) |>> fun two -> EBinOp (one, NotEqual, two)
                attempt (strWs ">" >>. parseExprWs) |>> fun two -> EBinOp (one, Greater, two)
                attempt (strWs ">=" >>. parseExprWs) |>> fun two -> EBinOp (one, GreaterEqual, two)
                attempt (strWs "<" >>. parseExprWs) |>> fun two -> EBinOp (one, Lesser, two)
                attempt (strWs "<=" >>. parseExprWs) |>> fun two -> EBinOp (one, LesserEqual, two)
                attempt (strWs "." >>. identWs) |>> fun field -> ERecordSelect (one, field)
                attempt (strWs "\\" >>. identWs) |>> fun field -> ERecordRestrict (one, field)
                preturn one
            ]
        | _ -> 
            let rec loop state exprs =
                match state, exprs with
                | None, fn :: arg :: exprs -> loop (Some (ECall (fn, arg))) exprs
                | None, _ -> raise (parserError FunctionCallNoArg)
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
let inline readExpr input = readOrThrow (parseExpr .>> eof) input
let inline readExprList input : Expr list =
    let parser = (sepEndBy parseExpr ws) .>> eof
    readOrThrow parser input

// let parseTy, parseTyRef = createParserForwardedToRef ()

// let parseTyWs = parseTy .>> ws
// let parseTyListWs = sepBy parseTyWs (strWs ",")

// let parseTConst =
//     identWs |>> TConst

// let parseTApp : Parser<Ty> =
//     parseTyWs .>> strWs "[" .>>. parseTyListWs .>> strWs "]"
//     |>> TApp

// let parseTArrow : Parser<Ty> =
//     parseTyWs .>> strWs "->" .>>. parseTyWs
//     |>> TArrow

// let nameToIdMap = ref Map.empty

// let nameToId s : Ty =
//     let m = !nameToIdMap
//     m 
//     |> Map.tryFind s 
//     |> Option.defaultWith (fun () ->
//         let id = newGenVar ()
//         nameToIdMap := Map.add s id m
//         id
//     )

// let parseTVar : Parser<Ty> =
//     strWs "'" >>. identWs
//     |>> nameToId

// let parseTEmptyRecord : Parser<Ty> =
//     strWs "{" .>> strWs "}"
//     |>> fun _ -> TRecord TRowEmpty

// let parseTRowFields : Parser<(string * Ty) list> =
//     let field = identWs .>>. parseTyWs
//     sepBy1 field (strWs ",")

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
