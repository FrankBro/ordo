module Emit

open Error
open Expr
open Infer
open Util

let currentId = ref 0

let nextId () =
    let id = !currentId
    currentId := id + 1
    id

let getNewVar () =
    let id = nextId ()
    sprintf "_lua%d" id

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

let emitUnop op =
    match op with
    | Negative -> "-"

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
    | EFun (EVar name, body) ->
        let var = getNewVar ()
        sprintf "function(%s)\nlocal %s = %s\nreturn %s\nend" name var (emitExpr body) var
    | ELet (EVar name, value, body) ->
        let emittedValue = emitExpr value
        sprintf "local %s = %s\n%s" name emittedValue (emitExpr body)
    | ERecordSelect (record, field) ->
        sprintf "%s.%s" (emitExpr record) field
    | ERecordExtend (field, value, record) ->
        sprintf "table.add(%s, '%s', %s)" (emitExpr record) field (emitExpr value)
    | ERecordRestrict (record, field) ->
        sprintf "table.remove(%s, '%s')" (emitExpr record) field
    | ERecordEmpty -> "{}"
    | EVariant (name, value) ->
        sprintf "{ variant_name = '%s', variant_value = %s }" name (emitExpr value)
    // | ECase of Expr * (Pattern * Expr * Guard option) list * (Name * Expr) option
    | EIfThenElse (i, t, e) ->
        let var = getNewVar ()
        sprintf "local %s\nif %s then\n%s = %s\nelse %s = %s\nend" var (emitExpr i) var (emitExpr t) var (emitExpr e)
    | EBinOp (l, op, r) ->
        sprintf "%s %s %s" (emitExpr l) (emitBinop op) (emitExpr r)
    | EUnOp (op, e) ->
        sprintf "%s%s" (emitUnop op) (emitExpr e)
    | EFix name -> sprintf "%s()" name
    // | EListEmpty
    // | EListCons of Expr * Expr
    // | EOpen of string
    // | EType of Expr * Ty
    | EPrint e -> sprintf "print(%s)" (emitExpr e)
    | _ -> failwithf "impossible, got %O" expr

let emitPrelude =
    [
        "function table.copy(t)"
        "    local u = {}"
        "    for k, v in pairs(t) do u[k] = v end"
        "    return u"
        "end"
        "function table.add(t, k, v)"
        "    local u = table.copy(t)"
        "    u[k] = v"
        "    return u"
        "end"
        "function table.remove(t, k)"
        "    local u = table.copy(t)"
        "    u[k] = nil"
        "    return u"
        "end"
        "function table.inspect(t)"
        "    for k, v in pairs(t) do print(k, v) end"
        "end"
    ]
    |> String.concat " "

let emit expr =
    emitPrelude + "\n\n" + emitExpr expr