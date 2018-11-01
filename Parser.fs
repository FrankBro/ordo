module Parser

open System 

open FParsec
open FParsec.CharParsers
open FParsec.Primitives

open Error
open Expr
open Infer
open Util 

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

let reserved = [ "let"; "in"; "fun"; "match"; "if"; "then"; "else"; "true"; "false"; "when"; "fix"; "rec" ]

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
    many1 digit |>> (Array.ofList >> String >> int >> EInt)

let parseFloat : Parser<Expr> = 
    let pa = many1 digit |>> (Array.ofList >> String)
    let pb = str "." >>. many digit |>> (Array.ofList >> String)
    pipe2 pa pb (fun whole decimal -> 
        whole + "." + decimal |> float
        |> EFloat
    )

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
        ELet (pattern, value, body)
    )

let parseLetRec = 
    let p1 = strWs1 "let rec" >>. many1 parsePatternWs
    let p2 = strWs "=" >>. parseExprWs
    let p3 = opt (strWs1 "in" >>. parseExprWs)
    pipe3 p1 p2 p3 (fun patterns value oBody -> 
        let name, value =
            match patterns with
            | [pattern] -> raise (parserError InvalidLetRec)
            | EVar name :: rest ->
                let wrappedValue =
                    (rest, value)
                    ||> List.foldBack (fun pattern state ->
                        EFun (pattern, state)
                    )
                name, wrappedValue
            | _ -> raise (parserError InvalidFunctionDeclaration)
        let body =
            oBody
            |> Option.defaultWith (fun () ->
                match patterns with
                | [] -> raise (parserError InvalidFunctionDeclaration)
                | pattern :: _ -> pattern
            )
        ELet (EVar ("_" + name), EFun (EVar name, value), ELet (EVar name, EFix ("_" + name), body))
    )

let parseVariant =
    strWs ":" >>. identWs .>>. parseExprWs
    |>> EVariant

let parseMatchNormalCase : Parser<Pattern * Expr * Guard option> =
    let pa = parsePatternWs
    let pb = opt (strWs "when" >>. parseExprWs)
    let pc = strWs "->" >>. parseExprWs
    pipe3 pa pb pc (fun pattern guard expr -> (pattern, expr, guard))

let parseMatchDefaultCase : Parser<string * Expr> =
    let pa = identWs
    let pb = strWs "->" >>. parseExprWs
    pipe2 pa pb (fun var expr -> (var, expr))

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

type RecordAction =
    | Set 
    | Update 

let parseRecordLabelOrUpdate : Parser<string * Expr * RecordAction> =
    (attempt (identWs .>> strWs ":=" .>>. parseExprWs) |>> (fun (label, expr) -> label, expr, Update))
    <|> (parseRecordLabel parseExprWs |>> (fun (label, expr) -> label, expr, Set))

let parseRecordLabels content : Parser<(string * Expr) list> =
    sepBy1 (parseRecordLabel content) (strWs ",")

let parseRecordLabelsOrUpdate =
    sepBy1 (parseRecordLabelOrUpdate) (strWs ",")

let parseRecordExtendPattern =
    let p1 = strWs "{" >>. (parseRecordLabels parsePatternWs .>> ws)
    let p2 = strWs "|" >>. parseExprWs .>> strWs "}"
    pipe2 p1 p2 exprRecordExtend

let parseRecordExtendOrUpdate =
    let p1 = strWs "{" >>. (parseRecordLabelsOrUpdate .>> ws)
    let p2 = strWs "|" >>. parseExprWs .>> strWs "}"
    pipe2 p1 p2 (fun labelsOrUpdate record ->
        let labels =
            labelsOrUpdate
            |> List.map (fun (label, expr, _) -> label, expr)
        let toRemove =
            labelsOrUpdate
            |> List.choose (fun (label, _, action) ->
                match action with
                | Set -> None
                | Update -> Some label
            )
        let restrictedRecord =
            (record, toRemove)
            ||> List.fold (fun record label ->
                ERecordRestrict (record, label)
            )
        exprRecordExtend labels restrictedRecord
    )

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

let parseFix =
    strWs1 "fix" >>. identWs |>> EFix

do parsePatternRef :=
    choice [
        parseParen parsePatternWs
        parseVariant
        attempt parseVar
        attempt parseRecordEmpty
        attempt parseRecordExtendPattern 
        attempt (parseRecordInit parsePatternWs)
    ]

let parseListEmpty =
    strWs "[]" |>> fun _ -> EListEmpty

let parseListLiteral = 
    strWs "[" >>. sepBy1 parseExprWs (strWs ",") .>> strWs "]"
    |>> fun list ->
        (EListEmpty, list)
        ||> List.fold (fun state x ->
            EListCons (x, state)
        )

let parseNotCallOrRecordSelect =
    choice [
        attempt parseListEmpty
        parseListLiteral
        parseParen parseExprWs
        attempt parseLetRec
        attempt parseFix
        parseBool
        attempt parseFloat
        attempt parseInt
        parseFun
        parseLet
        attempt parseVar
        attempt parseVariant
        parseMatch
        attempt parseRecordEmpty
        attempt parseRecordExtendOrUpdate 
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

let notArrow : Parser<unit> = notFollowedBy (str ">") >>. ws

opp.AddOperator(PrefixOperator("-", notArrow, 8, true, fun a -> EUnOp (Negative, a)))

opp.AddOperator(InfixOperator("*", ws, 7, Associativity.Left, fun a b -> EBinOp (a, Multiply, b)))
opp.AddOperator(InfixOperator("/", ws, 7, Associativity.Left, fun a b -> EBinOp (a, Divide, b)))


opp.AddOperator(InfixOperator("+", ws, 6, Associativity.Left, fun a b -> EBinOp (a, Plus, b)))
opp.AddOperator(InfixOperator("-", notArrow, 6, Associativity.Left, fun a b -> EBinOp (a, Minus, b)))

opp.AddOperator(InfixOperator("::", ws, 5, Associativity.Right, fun a b -> EListCons (a, b)))
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

let typeReserved = [ "forall" ]

let parseTConst: Parser<Ty> =
    many1 lower |>> (Array.ofList >> String)
    >>= fun s ->
        if typeReserved |> List.exists ((=) s) then 
            fail "reserved"
        else
            TConst s
            |> preturn

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
            | TList ty -> TList (f ty)
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

let parseTypeParen =
    strWs "(" >>. parseTyWs .>> strWs ")"

let parseNotLeftRecursiveType = 
    choice [
        parseTypeParen
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
