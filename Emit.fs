module Emit

open Error
open Expr
open Infer
open Util

let emitBinop op =
    match op with
    | Plus -> "+"
    | Minus -> "-"
    | Multiply -> "*"
    | Divide -> "/"
    | And -> "and"
    | Or -> "or"
    | Equal -> "=="
    | NotEqual -> "~="
    | Greater -> ">"
    | GreaterEqual -> ">="
    | Lesser -> "<"
    | LesserEqual -> "<="

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
    | ECall (fn, arg) ->
        sprintf "%s(%s)" (emitExpr fn) (emitExpr arg)
    | EFun (pattern, body) ->
        sprintf "function(%s) return %s end" (emitPattern pattern) (emitExpr body)
    | ELet (pattern, value, body) ->
        sprintf "local %s = %s\n%s" (emitPattern pattern) (emitExpr value) (emitExpr body)
    // | ERecordSelect of Expr * Name
    // | ERecordExtend of Name * Expr * Expr
    // | ERecordRestrict of Expr * Name
    // | ERecordEmpty
    // | EVariant of Name * Expr
    // | ECase of Expr * (Pattern * Expr * Guard option) list * (Name * Expr) option
    // | EIfThenElse of Expr * Expr * Expr
    | EBinOp (l, op, r) ->
        sprintf "%s %s %s" (emitExpr l) (emitBinop op) (emitExpr r)
    // | EUnOp of UnOp * Expr
    // | EFix of Name
    // | EListEmpty
    // | EListCons of Expr * Expr
    // | EOpen of string
    // | EType of Expr * Ty
    | EPrint e -> sprintf "print(%s)" (emitExpr e)
    | _ -> failwith "impossible"
