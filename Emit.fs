module Emit

open System.IO

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

let rec isSingleLineExpr expr =
    match expr with
    | ETyped (e, _) -> isSingleLineExpr e
    | EFfi _
    | EPrint _
    | EDebug _
    | EBool _
    | EInt _
    | EFloat _
    | EChar _
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
    | ESprintf _
    | EError _ -> true
    | EFun _
    | ELet _
    | ESet _
    | EFor _
    | EIfThenElse _ -> false
    | ECase _
    | EListEmpty
    | EListCons _
    | EType _ -> failwith "TODO"

let isStatement expr =
    match expr with
    | EDebug _
    | EPrint _
    | EBool _
    | EInt _
    | EFloat _
    | EChar _
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
    | EFfi _
    | ESprintf _
    | EType _
    | EError _ -> false
    | ELet _
    | ECase _
    | ESet _
    | EFor _
    | EIfThenElse _ -> true
    | EListEmpty
    | EListCons _
    | ETyped _ -> failwith "TODO"

let rec extractBindingsAndVariantGuards pattern var bindings guards =
    match pattern with
    | EVar name -> Map.add name var bindings, guards
    | ERecordEmpty -> bindings, guards
    | ERecordExtend (field, fieldPattern, record) ->
        let fieldAccess = sprintf "%s.%s" var field
        let map, guards = extractBindingsAndVariantGuards fieldPattern fieldAccess bindings guards
        extractBindingsAndVariantGuards record var map guards
    | EVariant (name, variant) ->
        let fieldAccess = sprintf "%s.variant_%s" var name
        let guard = sprintf "%s.variant_name == '%s'" var name
        let guards = guard :: guards
        extractBindingsAndVariantGuards variant fieldAccess bindings guards
    | _ -> failwithf "impossible, got pattern %O" pattern

let emitFfi f =
    match f with
    | FileReadLines filename -> sprintf "file_lines('%s')" filename
    | FileRead filename -> sprintf "file_read('%s')" filename

let extractValueGuard guard bindings =
    match guard with
    | EVar name -> Map.find name bindings
    | EBool b -> string b
    | EInt i -> string i
    | EFloat f -> string f
    | EString s -> sprintf "'%s'" s
    | _ -> failwithf "impossible, got guard %O" guard

let rec emitCaseExpr oAssignVar map value (cases: (Pattern * Expr * Guard option) list) (oDefault: (Name * Expr) option) =
    let valueVar =
        match value with
        | EVar name -> name
        | _ -> failwith "value is not a var"
    let assignVar =
        match oAssignVar with
        | Some var -> var
        | None -> getNewVar ()
    let cases =
        cases
        |> List.map (fun (pattern, body, oGuard) ->
            let bindings, guards = extractBindingsAndVariantGuards pattern valueVar Map.empty []
            let guards =
                match oGuard with
                | None -> guards
                | Some guard -> 
                    emitExpr None bindings guard :: guards
            let guard =
                match guards with
                | [] -> failwith "invalid, will always match"
                | guards -> 
                    guards
                    |> String.concat " and "
            sprintf "if %s then\n%s\n" guard (emitExpr (Some assignVar) bindings body)
        )
        |> String.concat "else"
    let def =
        match oDefault with
        | Some (var, body) -> 
            sprintf "else\nlocal %s = %s\n%s\nend" var valueVar (emitExpr (Some var) map body)
        | None -> sprintf "else\nerror('no match')\nend"
    match oAssignVar with
    | Some _ -> cases + def
    | None -> sprintf "local %s\n%s%s" assignVar cases def

and emitExpr oAssignVar map expr =
    let getVar var =
        map
        |> Map.tryFind var
        |> Option.defaultValue var
    match expr with
    | ETyped (expr, _) ->
        emitExpr oAssignVar map expr
    | EType (name, ty, body) ->
        let ty = stringOfTy ty
        let body = emitExpr None map body
        sprintf "local %s = '%s'\n%s" name ty body
    | EOpen name -> sprintf "require('output.%s')" name
    | EListEmpty -> failwith "TODO"
    | EListCons _ -> failwith "TODO"
    | EError s -> sprintf "error('%s')" s
    | ESprintf (s, args) -> 
        let args  =
            args
            |> List.map (emitExpr None map)
            |> String.concat ", "
        sprintf "string.format('%s', %s)" s args
    | EFfi f -> emitFfi f
    | EBool false -> "false"
    | EBool true -> "true"
    | EInt i -> string i
    | EFloat f -> string f
    | EChar c -> sprintf "'%s'" (string c)
    | EString s -> sprintf "'%s'" s
    | EVar name -> getVar name
    | ECall (fn, arg) ->
        sprintf "%s(%s)" (emitExpr None map fn) (emitExpr None map arg)
    | EFun (EVar name, body) ->
        let var = getNewVar ()
        sprintf "function(%s)\nlocal %s\n%s\nreturn %s\nend" name var (emitExpr (Some var) map body) var
    | EFun _ -> failwith "TODO"
    | ELet (EVar name, value, body) ->
        let body =
            match body with
            | ERecordEmpty -> ""
            | _ -> emitExpr oAssignVar map body
        match oAssignVar with
        | None when isStatement value ->
            sprintf "local %s\n%s\n%s" name (emitExpr (Some name) map value) body
        | Some assignName when assignName = name -> 
            sprintf "%s = %s\n%s" name (emitExpr None map value) body
        | _ ->
            sprintf "local %s = %s\n%s" name (emitExpr None map value) body
    | ELet _ -> failwith "TODO"
    | ESet (name, value, body) ->
        let body =
            match body with
            | ERecordEmpty -> ""
            | _ -> emitExpr None map body
        sprintf "%s = %s\n%s" name (emitExpr None map value) body
    | ERecordSelect (record, field) ->
        sprintf "%s.%s" (emitExpr None map record) field
    | ERecordExtend (field, value, record) ->
        sprintf "table.add(%s, '%s', %s)" (emitExpr None map record) field (emitExpr None map value)
    | ERecordRestrict (record, field) ->
        sprintf "table.remove(%s, '%s')" (emitExpr None map record) field
    | ERecordEmpty -> "{}"
    | EVariant (name, value) ->
        sprintf "{ variant_name = '%s', variant_%s = %s }" name name (emitExpr None map value)
    | ECase (value, cases, oDefault) ->
        emitCaseExpr oAssignVar map value cases oDefault
    | EFor (key, value, target, body, rest) ->
        let var = getNewVar ()
        let prepareLine = sprintf "%s = prepare_for(%s)" var (emitExpr None map target)
        let forLine = sprintf "for %s, %s in pairs(%s) do" key value var
        let body = emitExpr None map body
        let rest = 
            match rest with
            | ERecordEmpty -> ""
            | _ -> emitExpr None map rest
        sprintf "%s\n%s\n%s\nend\n%s" prepareLine forLine body rest
    | EIfThenElse (i, t, e) ->
        match oAssignVar with
        | None ->
            let var = getNewVar ()
            sprintf "local %s\nif %s then\n%s\nelse\n%s\nend" var (emitExpr None map i) (emitExpr (Some var) map t) (emitExpr (Some var) map e)
        | Some var ->
            let var = getVar var
            sprintf "if %s then\n%s\nelse\n%s\nend" (emitExpr None map i) (emitExpr (Some var) map t) (emitExpr (Some var) map e)
    | EBinOp (l, op, r) ->
        sprintf "%s %s %s" (emitExpr None map l) (emitBinop op) (emitExpr None map r)
    | EUnOp (op, e) ->
        sprintf "%s%s" (emitUnop op) (emitExpr None map e)
    | EFix name -> sprintf "fix(%s)" name
    // | EListEmpty
    // | EListCons of Expr * Expr
    // | EOpen of string
    // | EType of Expr * Ty
    | EPrint (e, body) -> 
        let body =
            match body with
            | ERecordEmpty -> ""
            | _ -> emitExpr None map body
        sprintf "print(%s)\n%s" (emitExpr None map e) body
    | EDebug (e, body) ->
        let e =
            match !e with
            | ETyped (e, ty) ->  sprintf "%s, \"%s\"" (emitExpr None map e) (stringOfTy ty)
            | _ -> raise (compilerError DebugNotTyped)
        let body =
            match body with
            | ERecordEmpty -> ""
            | _ -> emitExpr None map body
        sprintf "print(%s)\n%s" e body

    |> (fun result ->
        match oAssignVar with
        // | Some assignVar when isSingleLineExpr expr -> sprintf "%s = %s" assignVar result
        | Some assignVar when not (isStatement expr) ->
            let assignVar = getVar assignVar
            sprintf "%s = %s" assignVar result
        | _ -> result
    )

let emit expr =
    "local std = require('output.std')" + "\n" + emitExpr None Map.empty expr
