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
    | EFun (pattern, body) ->
        let emittedPattern = emitFunPattern pattern
        sprintf "%s return %s end" emittedPattern (emitExpr body)
    | ELet (pattern, value, body) ->
        let emittedValue = emitExpr value
        let emittedPattern = emitLetPattern pattern emittedValue
        sprintf "%s\n%s" emittedPattern (emitExpr body)
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
        sprintf "if %s then %s else %s end" (emitExpr i) (emitExpr t) (emitExpr e)
    | EBinOp (l, op, r) ->
        sprintf "%s %s %s" (emitExpr l) (emitBinop op) (emitExpr r)
    | EUnOp (op, e) ->
        sprintf "%s%s" (emitUnop op) (emitExpr e)
    // | EFix of Name
    // | EListEmpty
    // | EListCons of Expr * Expr
    // | EOpen of string
    // | EType of Expr * Ty
    | EPrint e -> sprintf "print(%s)" (emitExpr e)
    | _ -> failwith "impossible"

and emitFunPattern pattern =
    match pattern with
    | EVar name -> sprintf "function(%s)" name
    | _ -> failwithf "impossible, got %O" pattern

and emitLetPattern _ _ = failwith ""
// and emitLetPattern pattern value =
//     match pattern with
//     | EVar name -> sprintf "local %s = %s" name value
//     | ERecordExtend (field, fieldValue, record) ->
//         let recordLine = 
//             match record with
//             | ERecordEmpty -> sprintf "local temp_record = %s" value
//             | record -> emitLetPattern record value
//         let fieldLine = sprintf "local %s = %s\n%s" field (emitLetPattern fieldValue)
//         sprintf "%s\n%s" recordLine fieldLine

        
//     | _ -> failwithf "impossible, got %O" pattern

        // | EType (e, _) -> loop env e value
        // | EListEmpty -> VList [] = value, env
        // | EListCons (x, xs) ->
        //     match value with
        //     | VList [] ->
        //         false, env
        //     | VList (xValue :: xsValue) ->
        //         let xValid, env = loop env x xValue
        //         let xsValid, env = loop env xs (VList xsValue)
        //         xValid && xsValid, env
        //     | _ ->
        //         raise (evalError InvalidList)
        // | EBool b -> VBool b = value, env
        // | EInt i -> VInt i = value, env
        // | EFloat f -> VFloat f = value, env
        // | EString s -> VString s = value, env
        // | EVar var -> true, Map.add var value env
        // | ERecordEmpty -> true, env
        // | ERecordExtend (label, expr, record) ->
        //     match value with
        //     | VRecord fields ->
        //         let field = 
        //             fields
        //             |> Map.tryFind label
        //             |> Option.defaultWith (fun () ->
        //                 raise (genericError (FieldNotFound label))
        //             )
        //         let matches, env = evalPattern env expr field
        //         if not matches then
        //             false, initialEnv
        //         else
        //             let remainingFields =
        //                 fields
        //                 |> Map.remove label
        //             loop env record (VRecord remainingFields)
        //     | _ ->
        //         raise (genericError (NotARecordValue value))
        // | EVariant (label, expr) ->
        //     match value with
        //     | VVariant (name, value) when label = name ->
        //         let matches, env = evalPattern env expr value
        //         matches, env
        //     | VVariant (name, _) ->
        //         raise (evalError (BadVariantPattern (label, name)))
        //     | _ ->
        //         raise (genericError (NotAVariantValue value))
        // | _ -> 
        //     raise (genericError (InvalidPattern pattern))

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
