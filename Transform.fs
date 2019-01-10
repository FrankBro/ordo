module Transform

open Error
open Expr

let currentId = ref 0

let nextId () =
    let id = !currentId
    currentId := id + 1
    id

let getNewVar () =
    let id = nextId ()
    sprintf "_var%d" id

let rec transformRowBinding var body expr =
    match expr with
    | EVar _  -> body
    | EVariant (_, EVar _) -> 
        ECase (EVar var, [expr, body, None], None)
    | EVariant (case, sub) ->
        let subVar = getNewVar ()
        let subBody = transformRowBinding subVar body sub
        ECase (EVar var, [EVariant (case, EVar subVar), subBody, None], None)
    | ERecordExtend (name, EVar nameVar, record) ->
        let body = transformRowBinding var body record 
        ELet (EVar nameVar, ERecordSelect (EVar var, name), body)
    | ERecordExtend (name, sub, record) ->
        let recordBody = transformRowBinding var body record
        let subVar = getNewVar ()
        let subBody = transformRowBinding subVar recordBody sub
        // let subVar = getNewVar ()
        // let subBody = transformRowBinding subVar record sub
        // let recordBody = transformRowBinding var body subBody
        ELet (EVar subVar, ERecordSelect (EVar var, name), subBody)
    | ERecordEmpty -> body
    | _ -> raise (genericError (InvalidPattern expr))

let rec transformExpr expr =
    match expr with
    | EFun (pattern, body) ->
        match pattern with
        | EVar _  -> EFun (pattern, expr)
        | EVariant _ 
        | ERecordExtend _ -> 
            let var = getNewVar ()
            let body = transformRowBinding var body pattern
            EFun (EVar var, body)
        | _ -> raise (genericError (InvalidPattern pattern))
    | ELet (pattern, value, body) ->
            let var = getNewVar ()
            let body = transformRowBinding var body pattern
            ELet (EVar var, value, body)
    // | ECase of Expr * (Pattern * Expr * Guard option) list * (Name * Expr) option
    | _ -> expr

let transform expr =
    currentId := 0
    transformExpr expr
