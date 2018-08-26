module Eval

open Error
open Expr
open Util

type Value =
    | VBool of bool
    | VInt of int
    | VFloat of float
    | VFun of Name * Expr
    | VRecord of Map<Name, Value>
    | VVariant of Name * Value

let rec stringOfValue value =
    let rec f isSimple value =
        match value with
        | VBool b -> string b
        | VInt i -> string i
        | VFloat f -> string f
        | VFun _ -> "<Lambda>"
        | VRecord fields ->
            fields
            |> Map.toList
            |> List.map (fun (label, value) -> sprintf "%s : %s" label (stringOfValue value))
            |> String.concat ", "
            |> sprintf "{ %s }"
        | VVariant (label, value) -> sprintf ":%s %s" label (stringOfValue value)
    f false value

let rec evalExpr (env: Map<string, Value>) (expr: Expr) : Value =
    match expr with
    | EBool b -> VBool b
    | EInt i -> VInt i
    | EFloat f -> VFloat f
    | EFun (name, expr) -> VFun (name, expr)
    | EVar name -> 
        Map.tryFind name env
        |> Option.defaultWith (fun () ->
            raise (genericError (VariableNotFound name))
        )
    | ECall (fnExpr, argExpr) -> 
        let fnValue = evalExpr env fnExpr
        match fnValue with
        | VFun (argName, bodyExpr) ->
            let initialEnv = env
            let argValue = evalExpr initialEnv argExpr
            let fnEnv = Map.add argName argValue env
            evalExpr fnEnv bodyExpr
        | _ -> raise (evalError (NotAFunction fnExpr))
    | ELet (name, valueExpr, bodyExpr) ->
        let value = evalExpr env valueExpr
        let env = Map.add name value env
        evalExpr env bodyExpr
    | ERecordEmpty  -> VRecord Map.empty
    | EVariant (label, expr) ->
        let value = evalExpr env expr
        VVariant (label, value)
    | ERecordExtend (name, valueExpr, recordExpr) ->
        let recordValue = evalExpr env recordExpr
        match recordValue with
        | VRecord fields ->
            let valueValue = evalExpr env valueExpr
            fields
            |> Map.add name valueValue
            |> VRecord
        | _ -> raise (evalError (NotARecord recordExpr))
    | ERecordRestrict (recordExpr, name) ->
        let recordValue = evalExpr env recordExpr
        match recordValue with
        | VRecord fields ->
            fields
            |> Map.remove name
            |> VRecord
        | _ -> raise (evalError (NotARecord recordExpr))
    | ERecordSelect (recordExpr, label) -> 
        let recordValue = evalExpr env recordExpr
        match recordValue with
        | VRecord fields ->
            fields
            |> Map.tryFind label
            |> Option.defaultWith (fun () ->
                raise (genericError (FieldNotFound label))
            )
        | _ -> raise (evalError (NotARecord recordExpr))
    | ECase (valueExpr, cases, oDefault) ->
        let valueValue = evalExpr env valueExpr
        match valueValue with
        | VVariant (label, value) ->
            let var, expr =
                cases
                |> List.tryFind (fun (caseLabel, _, _) -> caseLabel = label)
                |> Option.map (fun (_, var, expr) -> var, expr)
                |> Option.defaultWith (fun () ->
                    oDefault
                    |> Option.defaultWith (fun () ->
                        raise (evalError (MissingMatchCase valueExpr))
                    )
                )
            let initialEnv = env
            let fnEnv = Map.add var value env
            evalExpr fnEnv expr
        | _ -> 
            raise (evalError (NotAVariant valueExpr))
    | EIfThenElse (ifExpr, thenExpr, elseExpr) ->
        let ifValue = evalExpr env ifExpr
        match ifValue with
        | VBool true -> evalExpr env thenExpr
        | VBool false -> evalExpr env elseExpr
        | _ -> 
            raise (genericError IfValueNotBoolean )

let eval expr : Value =
    evalExpr Map.empty expr
