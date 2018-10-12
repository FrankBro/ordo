module Pattern

open Error
open Expr

// How to match somethng like this:
// case x of { Foo { a = Bar b }} -> 1 | otherwise -> 0 }
//
// This file will be used to translate patterns into more expressions. Some examples:
// let { a = a, b = b | rest } = { a = 1, b = 2, c = 3 } in a
// let temp1 = { a = 1, b = 2, c = 3 } in let a = temp1.a in let b = temp1.b in let rest = temp1\a\b in a
//
// For value matching, the pattern expression will need to be enhanced to have an extra condition expression to be executed
// case Foo { a = 1, b = 2 } of { Foo { a = 1, b = b } -> b, Foo _ -> 0 }
// case Foo { a = 1, b = 2 } of { Foo temp1 when temp1.a = 1 } -> let b = temp1.b in b, Foo _ -> 0 }

let getRecordLabelExpr label expr =
    let rec loop expr =
        match expr with
        | ERecordEmpty ->
            raise (genericError (FieldNotFound label))
        | ERecordExtend (recordLabel, expr, _) when label = recordLabel ->
            expr
        | ERecordExtend (_, _, record) ->
            loop record
        | ERecordRestrict (record, _) ->
            loop record
        | _ ->
            raise (genericError (NotARecordExpr expr))
    loop expr

let getRecordLabelType label ty =
    let rec loop ty =
        printfn "DEBUG: ty = %O" ty
        match ty with
        | TRowEmpty ->
            raise (genericError (FieldNotFound label))
        | TRowExtend (recordLabel, ty, _) when label = recordLabel ->
            ty
        | TRowExtend (_, _, record) ->
            loop record
        | TRecord record ->
            loop record
        | TVar {contents = Link ty} ->
            loop ty
        | _ ->
            raise (genericError (NotARecordTy ty))
    loop ty

let removeRecordLabelExpr label expr =
    let rec loop acc expr =
        match expr with
        | ERecordEmpty -> 
            ERecordEmpty
        | ERecordExtend (recordLabel, expr, record) when label = recordLabel ->
            loop acc record
        | ERecordExtend (label, expr, record) ->
            loop (ERecordExtend (label, expr, acc)) record
        | ERecordRestrict (record, recordLabel) when label = recordLabel ->
            loop acc record
        | ERecordRestrict (record, label) ->
            loop acc record
        | _ ->
            raise (genericError (NotARecordExpr expr))
    loop ERecordEmpty expr

let recordToRecordMapTy ty =
    let rec loop acc ty =
        printfn "DEBUG: ty = %O" ty
        match ty with
        | TRowEmpty -> acc
        | TRowExtend (label, ty, record) ->
            loop (Map.add label ty acc) record
        | TRecord record ->
            loop acc record
        | TVar {contents = Link ty} ->
            loop acc ty
        | _ ->
            raise (genericError (NotARecordTy ty))
    loop Map.empty ty

let recordMapToRecordTy (map: Map<Name, Ty>) =
    (TRowEmpty, map)
    ||> Map.fold (fun record label ty ->
        TRowExtend (label, ty, record)
    )
    |> TRecord

let removeRecordLabelTy label ty : Ty =
    recordToRecordMapTy ty
    |> Map.remove label
    |> recordMapToRecordTy

let recordToMapExpr expr =
    let rec loop acc expr =
        match expr with
        | ERecordEmpty -> acc
        | ERecordExtend (label, EVar name, record) ->
            let acc = Map.add label name acc
            loop acc record
        | _ ->
            raise (genericError (InvalidPattern expr))
    loop Map.empty expr
