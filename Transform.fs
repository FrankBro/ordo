module Transform

open Error
open Expr

// Before
// let { a = 1 } = { a = 1 } in 1
// After
// let _var0 = { a = 1 } in let _var1 = _var0.a in if a = 1 then 1 else error "bad match"

// Before
// match a { :a 1 -> 1 }
// After
//

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
    | EBool b -> failwith ""
    | EInt i -> failwith ""
    | EFloat f -> failwith ""
    | EString s -> failwith ""
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
        ELet (EVar subVar, ERecordSelect (EVar var, name), subBody)
    | ERecordEmpty -> body
    | _ -> raise (genericError (InvalidPattern expr))

let isValueType expr =
    match expr with
    | EBool _
    | EInt _
    | EFloat _ 
    | EString _ -> true
    | _ -> false

let extractValueGuard var expr =
    match expr with
    | EBool _
    | EInt _
    | EFloat _
    | EString _ -> EBinOp (EVar var, BinOp.Equal, expr)
    | _ -> raise (genericError (InvalidPattern expr))

let rec extractGuards pattern guards =
    match pattern with
    | EVar _  -> pattern, guards
    | EVariant (_, EVar _) -> pattern, guards
    | EVariant (case, body) when isValueType body -> 
        let var = getNewVar ()
        let guard = extractValueGuard var body
        EVariant (case, EVar var), guard :: guards
    | EVariant (case, body) ->
        let body, guards = extractGuards body guards
        EVariant (case, body), guards
    | ERecordExtend (_, EVar _, record) -> extractGuards record guards
    | ERecordExtend (name, body, record) when isValueType body ->
        let var = getNewVar ()
        let record, guards = extractGuards record guards
        let guard = extractValueGuard var body
        ERecordExtend (name, EVar var, record), guard :: guards
    | ERecordExtend (name, body, record) ->
        let body, guards = extractGuards body guards
        let record, guards = extractGuards record guards
        ERecordExtend (name, body, record), guards
    | ERecordEmpty -> pattern, guards
    | _ -> raise (genericError (InvalidPattern pattern))

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
    | ECase (value, cases, oDefault) ->
        let fixedCases = 
            cases
            |> List.map (fun (pattern, body, oGuard) ->
                let guards =
                    match oGuard with
                    | None -> []
                    | Some previousGuards -> [previousGuards]
                let pattern, guards = extractGuards pattern guards
                let oGuard =
                    match guards with
                    | [] -> None
                    | guard :: [] -> Some guard
                    | guard :: guards ->
                        (guard, guards)
                        ||> List.fold (fun state guard ->
                            EBinOp (state, BinOp.And, guard)
                        )
                        |> Some
                pattern, body, oGuard
            )
        ECase (value, fixedCases, oDefault)
    | _ -> expr

let transform expr =
    currentId := 0
    transformExpr expr
