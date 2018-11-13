module ParserType

open System 

open FParsec

open Expr
open Infer
open ParserUtil

let reserved = []

let ident: Parser<string> =
    many1 lower |>> (Array.ofList >> String)
    >>= fun s ->
        if reserved |> List.exists ((=) s) then 
            fail "reserved"
        else
            preturn s
let identWs = ident .>> ws

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
            | TBool | TInt | TFloat | TString -> ty
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
