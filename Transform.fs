module Transform

open Expr

// Expand pattern matching

// Before 
// let { a = a } = { a = 1 } in body
// After
// let _r0 = { a = 1 } in
// let a = _r0.a in body

// Before
// let (:a a) = (:a 1) in body
// After
// let _r0 = (:a 1) in
// match _r0 { :a a -> body }

// Before

let getId =
    let mutable i = 0
    fun () ->
        let j = i
        i <- j + 1
        j

let getNewVar () =
    let id = getId ()
    sprintf "_r%d" id

let transformRow var rest expr =
    match expr with
    | EVar _  -> expr
    | EVariant (name, expr) -> failwith ""

// let rec transformExpr expr =
//     match expr with
//     | EError _
//     | EPrint _
//     | EBool _
//     | EInt _
//     | EFloat _
//     | EString _
//     | EVar _ 
//     | EOpen _
//     | EFix _
//     | ERecordSelect _
//     | ECall _ -> expr
//     | EFun (pattern, body) ->
//         match pattern with
//         | EVar _ 
//         | EVariant _ -> EFun (pattern, expr)
//         | ERecordExtend _ ->
//             let var = getNewVar ()
//             let body = transformRow var body pattern
//             EFun (EVar var, body)
//         | _ -> raise (genericError (InvalidPattern pattern))
//     | ELet of Pattern * Expr * Expr
//     | ERecordExtend of Name * Expr * Expr
//     | ERecordRestrict of Expr * Name
//     | ERecordEmpty
//     | EVariant of Name * Expr
//     | ECase of Expr * (Pattern * Expr * Guard option) list * (Name * Expr) option
//     | EIfThenElse of Expr * Expr * Expr
//     | EBinOp of Expr * BinOp * Expr
//     | EUnOp of UnOp * Expr
//     | EListEmpty
//     | EListCons of Expr * Expr
//     | EType (e, ty) -> EType (transformExpr e, ty)
