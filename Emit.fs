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

let isSingleLineExpr expr =
    match expr with
    | EPrint _
    | EBool _
    | EInt _
    | EFloat _
    | EString _
    | EVar _
    | ECall _
    | ERecordSelect _
    | ERecordExtend _
    | ERecordRestrict _
    | ERecordEmpty 
    | EVariant _
    | EFix _
    | EBinOp _
    | EUnOp _
    | EOpen _
    | EError _ -> true
    | EFun _
    | ELet _
    | EIfThenElse _ -> false
    | _ -> failwith "TODO"
    // | ECase of Expr * (Pattern * Expr * Guard option) list * (Name * Expr) option
    // | EListEmpty
    // | EListCons of Expr * Expr
    // | EType of Expr * Ty

let isStatement expr =
    match expr with
    | EPrint _
    | EBool _
    | EInt _
    | EFloat _
    | EString _
    | EVar _
    | ECall _
    | EFun _
    | ERecordSelect _
    | ERecordExtend _
    | ERecordRestrict _
    | ERecordEmpty
    | EVariant _
    | EBinOp _
    | EUnOp _
    | EFix _
    | EOpen _
    | EError _ -> false
    | ELet _
    | ECase _
    | EIfThenElse _ -> true
    // | EListEmpty
    // | EListCons of Expr * Expr
    // | EType of Expr * Ty

let rec extractCaseConditions case value =

let emitCaseExpr oAssignVar value cases oDefault =
    let valueVar =
        match value with
        | EVar name -> name
        | _ -> failwith "value is not a var"
    let cases =
        cases
        |> List.map (fun case ->
            case
        )
    let def =
        match oDefault with
        | Some var -> sprintf "else\nlocal var = "
        | None -> sprintf "else\nerror('no match')\nend"
    failwith ""

let rec emitExpr oAssignVar expr =
    match expr with
    | EBool false -> "false"
    | EBool true -> "true"
    | EInt i -> string i
    | EFloat f -> string f
    | EString s -> sprintf "'%s'" s
    | EVar name -> name
    | ECall (fn, arg) ->
        sprintf "%s(%s)" (emitExpr None fn) (emitExpr None arg)
    | EFun (EVar name, body) ->
        let var = getNewVar ()
        sprintf "function(%s)\nlocal %s\n%s\nreturn %s\nend" name var (emitExpr (Some var) body) var
    | ELet (EVar name, value, body) ->
        let emittedValue = emitExpr None value
        let emittedBody = emitExpr None body
        match oAssignVar with
        | None -> sprintf "local %s = %s\n%s" name emittedValue emittedBody
        | Some assignVar -> sprintf "%s = %s\n%s" name emittedValue emittedBody
    | ERecordSelect (record, field) ->
        sprintf "%s.%s" (emitExpr None record) field
    | ERecordExtend (field, value, record) ->
        sprintf "table.add(%s, '%s', %s)" (emitExpr None record) field (emitExpr None value)
    | ERecordRestrict (record, field) ->
        sprintf "table.remove(%s, '%s')" (emitExpr None record) field
    | ERecordEmpty -> "{}"
    | EVariant (name, value) ->
        sprintf "{ variant_name = '%s', variant_value = %s }" name (emitExpr None value)
    | ECase (value, cases, oDefault) ->
        emitCaseExpr oAssignVar value cases oDefault
    | EIfThenElse (i, t, e) ->
        match oAssignVar with
        | None -> 
            let var = getNewVar ()
            sprintf "local %s\nif %s then\n%s\nelse\n%s\nend" var (emitExpr None i) (emitExpr (Some var) t) (emitExpr (Some var) e)
        | Some var -> 
            sprintf "if %s then\n%s\nelse\n%s\nend" (emitExpr None i) (emitExpr (Some var) t) (emitExpr (Some var) e)
    | EBinOp (l, op, r) ->
        sprintf "%s %s %s" (emitExpr None l) (emitBinop op) (emitExpr None r)
    | EUnOp (op, e) ->
        sprintf "%s%s" (emitUnop op) (emitExpr None e)
    | EFix name -> sprintf "fix(%s)" name
    // | EListEmpty
    // | EListCons of Expr * Expr
    // | EOpen of string
    // | EType of Expr * Ty
    | EPrint e -> sprintf "print(%s)" (emitExpr None e)
    | _ -> failwithf "impossible, got %O" expr

    |> (fun result ->
        match oAssignVar with
        // | Some assignVar when isSingleLineExpr expr -> sprintf "%s = %s" assignVar result
        | Some assignVar when not (isStatement expr) -> sprintf "%s = %s" assignVar result
        | _ -> result
    )

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
        "function fix(f)"
        "    return function(...)"
        "        return (function(x) return x(x) end)(function(x) return f(function(y) return x(x)(y) end) end)(...)"
        "    end"
        "end"
    ]
    |> String.concat " "

let emit expr =
    emitPrelude + "\n\n" + emitExpr None expr