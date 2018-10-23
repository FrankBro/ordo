module Parser

open System 

open FParsec
open FParsec.CharParsers
open FParsec.Primitives

open Error
open Expr
open Infer
open Util 
open FParsec
open FParsec

type Parser<'t> = Parser<'t, unit>

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

let opp = new OperatorPrecedenceParser<Expr,unit, unit>()
let parseExpr = opp.ExpressionParser
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
    attempt (identWs .>> strWs "=" .>>. content)
    <|> (identWs |>> (fun label -> label, EVar label))

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

let parseExprOrCall =
    many1 (parseNotCallOrRecordSelect .>> ws)
    >>= fun result ->
        match result with
        | [one] -> preturn one
        | _ -> 
            let rec loop state exprs =
                match state, exprs with
                | None, fn :: arg :: exprs -> loop (Some (ECall (fn, arg))) exprs
                | None, _ -> raise (parserError FunctionCallNoArg)
                | Some fn, arg :: exprs -> loop (Some (ECall (fn, arg))) exprs
                | Some expr, [] -> expr
            let calls = loop None result
            preturn calls

opp.AddOperator(InfixOperator(".", ws, 9, Associativity.Left, fun a b -> 
    match b with 
    | EVar name -> ERecordSelect (a, name)
    | _ -> raise (parserError InvalidRecordSelect)
))
opp.AddOperator(InfixOperator("\\", ws, 8, Associativity.Left, fun a b -> 
    match b with 
    | EVar name -> ERecordRestrict (a, name)
    | _ -> raise (parserError InvalidRecordRestrict)
))
opp.AddOperator(InfixOperator("*", ws, 7, Associativity.Left, fun a b -> EBinOp (a, Multiply, b)))
opp.AddOperator(InfixOperator("/", ws, 7, Associativity.Left, fun a b -> EBinOp (a, Divide, b)))


opp.AddOperator(InfixOperator("+", ws, 6, Associativity.Left, fun a b -> EBinOp (a, Plus, b)))
let notArrow : Parser<unit> = notFollowedBy (str ">") >>. ws
opp.AddOperator(InfixOperator("-", notArrow, 6, Associativity.Left, fun a b -> EBinOp (a, Minus, b)))

opp.AddOperator(InfixOperator("<", ws, 5, Associativity.Left, fun a b -> EBinOp (a, Lesser, b)))
opp.AddOperator(InfixOperator("<=", ws, 5, Associativity.Left, fun a b -> EBinOp (a, LesserEqual, b)))
opp.AddOperator(InfixOperator(">", ws, 5, Associativity.Left, fun a b -> EBinOp (a, Greater, b)))
opp.AddOperator(InfixOperator(">=", ws, 5, Associativity.Left, fun a b -> EBinOp (a, GreaterEqual, b)))

opp.AddOperator(InfixOperator("=", ws, 4, Associativity.Left, fun a b -> EBinOp (a, Equal, b)))
opp.AddOperator(InfixOperator("<>", ws, 4, Associativity.Left, fun a b -> EBinOp (a, NotEqual, b)))

opp.AddOperator(InfixOperator("&&", ws, 3, Associativity.Left, fun a b -> EBinOp (a, And, b)))

opp.AddOperator(InfixOperator("||", ws, 2, Associativity.Left, fun a b -> EBinOp (a, Or, b)))

opp.TermParser <- parseExprOrCall

let inline readOrThrow (parser: Parser<'a>) input : 'a =
    match runParserOnString parser () "" input with
    | ParserResult.Success (result, state, pos) -> result
    | ParserResult.Failure (se, e, state) -> 
        failwith "Parser error" 
let inline readExpr input = readOrThrow (parseExpr .>> eof) input
let inline readExprList input : Expr list =
    let parser = (sepEndBy parseExpr ws) .>> eof
    readOrThrow parser input

let parseTy, parseTyRef = createParserForwardedToRef ()

let parseTyWs = parseTy .>> ws
let parseTyListWs = sepBy parseTyWs (strWs ",")

let parseTConst =
    identWs |>> TConst

let parseTBool =
    strWs "bool" |>> fun _ -> TBool

let parseTInt =
    strWs "int" |>> fun _ -> TInt

let parseTFloat =
    strWs "float" |>> fun _ -> TFloat

let identList = sepBy1 identWs ws1

let parseRowConstraint =
    sepBy1 identWs (strWs "\\")

let parseRowConstraints =
    sepBy1 parseRowConstraint (strWs ",")

let parseTvar =
    let p1 = strWs1 "forall" >>. identList 
    let p2 = opt (strWs "." >>. strWs "(" >>. parseRowConstraints .>> strWs ")")
    let p3 = strWs "=>" >>. parseTyWs
    pipe3 p1 p2 p3 (fun vars oConstraints rest ->
        let constraintEnv =
            match oConstraints with
            | None -> Map.empty
            | Some constraints ->
                (Map.empty, constraints)
                ||> List.fold (fun env constraints ->
                    match constraints with
                    | []
                    | [_] -> failwith "Impossible"
                    | name :: constraints ->
                        Map.add name constraints env
                )
        let env =
            (Map.empty, vars)
            ||> List.fold (fun env var ->
                let ty = 
                    if var.StartsWith "r" 
                    then newGenRowVar Set.empty
                    else newGenVar ()
                Map.add var ty env
            )
        let rec f ty = 
            match ty with
            | TConst name ->
                match Map.tryFind name env with
                | None -> ty
                | Some gen -> gen
            | TBool | TInt | TFloat -> ty
            | TVar _ -> ty
            | TApp(ty, tyArgs) -> TApp(f ty, List.map f tyArgs)
            | TArrow(param, ret) -> TArrow(f param, f ret)
            | TRecord row -> TRecord (f row)
            | TVariant row -> TVariant (f row)
            | TRowEmpty -> ty
            | TRowExtend(label, ty, row) ->
                TRowExtend(label, f ty, f row)
        f rest
    )

let parseTEmptyRecord : Parser<Ty> =
    strWs "{" .>> strWs "}"
    |>> fun _ -> TRecord TRowEmpty

let parseTRowFields : Parser<(string * Ty) list> =
    let field = identWs .>> strWs ":" .>>. parseTyWs
    sepBy1 field (strWs ",")

let parseTClosedRecord : Parser<Ty> =
    strWs "{" >>. parseTRowFields .>> strWs "}"
    |>> fun fields ->
        (TRowEmpty, fields)
        ||> List.fold (fun rest (label, ty) ->
            TRowExtend (label, ty, rest)
        )
        |> TRecord

let parseTOpenRecord : Parser<Ty> =
    strWs "{" >>. parseTRowFields .>> strWs "|" .>>. parseTyWs .>> strWs "}"
    |>> fun (fields, rest) ->
        (rest, fields)
        ||> List.fold (fun rest (label, ty) ->
            TRowExtend (label, ty, rest)
        )
        |> TRecord

let parseTClosedVariant : Parser<Ty> =
    strWs "<" >>. parseTRowFields .>> strWs ">"
    |>> fun fields ->
        (TRowEmpty, fields)
        ||> List.fold (fun rest (label, ty) ->
            TRowExtend (label, ty, rest)
        )
        |> TVariant

let parseTOpenVariant : Parser<Ty> =
    strWs "<" >>. parseTRowFields .>> strWs "|" .>>. parseTyWs .>> strWs ">"
    |>> fun (fields, rest) ->
        (rest, fields)
        ||> List.fold (fun rest (label, ty) ->
            TRowExtend (label, ty, rest)
        )
        |> TVariant

let parseNotLeftRecursiveType = 
    choice [
        attempt parseTBool
        attempt parseTInt
        attempt parseTFloat
        attempt parseTvar
        attempt parseTConst
        attempt parseTEmptyRecord
        attempt parseTClosedRecord
        attempt parseTOpenRecord
        attempt parseTClosedVariant
        attempt parseTOpenVariant
    ]

let parseAll =
    many1 (parseNotLeftRecursiveType .>> ws)
    >>= fun result ->
        match result with
        | [one] -> 
            choice [
                attempt (strWs "[" >>. sepBy1 parseTy (strWs ",") .>> strWs "]") 
                    |>> fun two -> TApp (one, two)
                attempt (strWs "->" >>. parseTyWs) 
                    |>> fun two -> TArrow (one, two)
                preturn one
            ]
        | _ -> 
            failwith "should never happen?"

do parseTyRef := parseAll

let inline readType input = readOrThrow (parseTy .>> eof) input
