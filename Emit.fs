module Emit

open Error
open Expr
open Infer
open Util

let emitPattern pattern =
    match pattern with
    | EVar name -> name
    | _ -> failwith "impossible"

let rec emitExpr expr =
    match expr with
    | EBool false -> "false"
    | EBool true -> "true"
    | EInt i -> string i
    | EFloat f -> string f
    | EString s -> sprintf "'%s'" s
    | EVar name -> name
    // | ECall of Expr * Expr
    // | EFun of Pattern * Expr
    | ELet (pattern, value, body) ->
        sprintf "local %s = %s\n%s" (emitPattern pattern) (emitExpr value) (emitExpr body)
    // | ERecordSelect of Expr * Name
    // | ERecordExtend of Name * Expr * Expr
    // | ERecordRestrict of Expr * Name
    // | ERecordEmpty
    // | EVariant of Name * Expr
    // | ECase of Expr * (Pattern * Expr * Guard option) list * (Name * Expr) option
    // | EIfThenElse of Expr * Expr * Expr
    // | EBinOp of Expr * BinOp * Expr
    // | EUnOp of UnOp * Expr
    // | EFix of Name
    // | EListEmpty
    // | EListCons of Expr * Expr
    // | EOpen of string
    // | EType of Expr * Ty
    | EPrint e -> sprintf "print(%s)" (emitExpr e)
    | _ -> failwith "impossible"
