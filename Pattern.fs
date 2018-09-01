module Pattern

open Error
open Expr

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
