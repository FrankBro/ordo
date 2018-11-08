module TestUtil

open Xunit

open Error
open Expr

type ParseResult =
    | POk of Expr
    | PSkip
    | PFail of OrdoError

type InferResult =
    | IOk of string
    | ISkip
    | IFail of OrdoError

type EvalResult =
    | EOk of Value
    | ESkip
    | EFail of OrdoError

let test input parserExpected inferExpected evalExpected =
    let parserResult = 
        try
            Parser.readExpr input
            |> POk
        with
        | OrdoException error -> PFail error
    if parserExpected <> PSkip then
        Assert.StrictEqual(parserExpected, parserResult)
    let inferResult =
        match parserResult with
        | PSkip -> failwith "Impossible"
        | PFail e -> IFail e
        | POk expr ->
            try
                Infer.infer expr
                |> stringOfTy
                |> IOk
            with
            | OrdoException error -> IFail error
    if inferExpected <> ISkip then
        Assert.StrictEqual(inferExpected, inferResult)
    let evalResult =
        match parserResult with
        | PSkip -> failwith "Impossible"
        | PFail e -> EFail e
        | POk expr ->
            try
                Eval.eval expr
                |> EOk
            with
            | OrdoException error -> EFail error
    if evalExpected <> ESkip then 
        Assert.StrictEqual(evalExpected, evalResult)

let g e = OrdoError.Generic e
let e e = OrdoError.Eval e
let i e = OrdoError.Infer e

let eRecord xs =
    (ERecordEmpty, xs)
    ||> List.fold (fun record (label, value) ->
        ERecordExtend (label, value, record)
    )

let tRecord xs =
    (TRowEmpty, xs)
    ||> List.fold (fun record (label, value) ->
        TRowExtend (label, value, record)
    )
    |> TRecord

let tVariant xs =
    (TVar {contents = Generic 0}, xs)
    ||> List.fold (fun variant (label, value) ->
        TRowExtend (label, value, variant)
    )
    |> TVariant

let vRecord xs =
    (Map.empty, xs)
    ||> List.fold (fun record (label, value) ->
        Map.add label value record
    )
    |> VRecord
