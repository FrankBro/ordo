module Eval

open Error
open Expr
open Util

let rec evalExpr (env: Map<string, Value>) (expr: Expr) : Value =
    match expr with
    | EBool b -> VBool b
    | EInt i -> VInt i
    | EFloat f -> VFloat f
    | EFun (pattern, expr) -> VFun (env, pattern, expr)
    | EVar name -> 
        Map.tryFind name env
        |> Option.defaultWith (fun () ->
            raise (genericError (VariableNotFound name))
        )
    | ECall (fnExpr, argExpr) -> 
        let fnValue = evalExpr env fnExpr
        match fnValue with
        | VFun (env, pattern, bodyExpr) ->
            let argValue = evalExpr env argExpr
            let fnEnv = evalPattern env pattern argValue
            evalExpr fnEnv bodyExpr
        | _ -> raise (evalError (NotAFunction fnExpr))
    | ELet (pattern, valueExpr, bodyExpr) ->
        let value = evalExpr env valueExpr
        let env = evalPattern env pattern value
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
        | _ -> raise (genericError (NotARecordExpr recordExpr))
    | ERecordRestrict (recordExpr, name) ->
        let recordValue = evalExpr env recordExpr
        match recordValue with
        | VRecord fields ->
            fields
            |> Map.remove name
            |> VRecord
        | _ -> raise (genericError (NotARecordExpr recordExpr))
    | ERecordSelect (recordExpr, label) -> 
        let recordValue = evalExpr env recordExpr
        match recordValue with
        | VRecord fields ->
            fields
            |> Map.tryFind label
            |> Option.defaultWith (fun () ->
                raise (genericError (FieldNotFound label))
            )
        | _ -> raise (genericError (NotARecordExpr recordExpr))
    | ECase (valueExpr, cases, oDefault) ->
        let valueValue = evalExpr env valueExpr
        let oCases = tryMakeVariantCases cases
        match valueValue, oCases with
        | VVariant (label, value), Some cases ->
            let pattern, expr =
                cases
                |> List.tryFind (fun (caseLabel, _, _) -> caseLabel = label)
                |> Option.map (fun (_, pattern, expr) -> pattern, expr)
                |> Option.defaultWith (fun () ->
                    oDefault
                    |> Option.map (fun (name, expr) -> EVar name, expr)
                    |> Option.defaultWith (fun () ->
                        raise (evalError (MissingMatchCase valueExpr))
                    )
                )
            let fnEnv = evalPattern env pattern value
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
    | EBinOp (a, op, b) ->
        let a = evalExpr env a
        match op with
        | And ->
            match a with
            | VBool true ->
                let b = evalExpr env b
                match b with
                | VBool b -> VBool b
                | _ -> 
                    raise (evalError BadBinOp)
            | VBool false -> VBool false
            | _ -> 
                raise (evalError BadBinOp)
        | Or ->
            match a with
            | VBool false ->
                let b = evalExpr env b
                match b with
                | VBool b -> VBool b
                | _ -> 
                    raise (evalError BadBinOp)
            | VBool true -> VBool true
            | _ -> 
                raise (evalError BadBinOp)
        | _ ->
            let b = evalExpr env b
            match a, op, b with
            | VInt a, Plus, VInt b -> VInt (a + b)
            | VFloat a, Plus, VFloat b -> VFloat (a + b)
            | VInt a, Minus, VInt b -> VInt (a - b)
            | VFloat a, Minus, VFloat b -> VFloat (a - b)
            | VInt a, Multiply, VInt b -> VInt (a * b)
            | VFloat a, Multiply, VFloat b -> VFloat (a * b)
            | VInt a, Divide, VInt b -> VInt (a / b)
            | VFloat a, Divide, VFloat b -> VFloat (a / b)
            | VBool a, Equal, VBool b -> VBool (a = b)
            | VInt a, Equal, VInt b -> VBool (a = b)
            | VFloat a, Equal, VFloat b -> VBool (a = b)
            | VBool a, NotEqual, VBool b -> VBool (a <> b)
            | VInt a, NotEqual, VInt b -> VBool (a <> b)
            | VFloat a, NotEqual, VFloat b -> VBool (a <> b)
            | VInt a, Greater, VInt b -> VBool (a > b)
            | VFloat a, Greater, VFloat b -> VBool (a > b)
            | VInt a, GreaterEqual, VInt b -> VBool (a >= b)
            | VFloat a, GreaterEqual, VFloat b -> VBool (a >= b)
            | VInt a, Lesser, VInt b -> VBool (a < b)
            | VFloat a, Lesser, VFloat b -> VBool (a < b)
            | VInt a, LesserEqual, VInt b -> VBool (a <= b)
            | VFloat a, LesserEqual, VFloat b -> VBool (a <= b)
            | _ -> 
                raise (evalError BadBinOp)

and evalPattern (env: Map<string, Value>) pattern (value: Value) =
    let rec loop env pattern (value: Value) =
        match pattern with
        | EVar var -> Map.add var value env
        | ERecordEmpty -> env
        | ERecordExtend (label, expr, record) ->
            match value with
            | VRecord fields ->
                let field = 
                    fields
                    |> Map.tryFind label
                    |> Option.defaultWith (fun () ->
                        raise (genericError (FieldNotFound label))
                    )
                let env = evalPattern env expr field
                let remainingFields =
                    fields
                    |> Map.remove label
                loop env record (VRecord remainingFields)
            | _ ->
                raise (genericError (NotARecordValue value))
        | EVariant (label, expr) ->
            match value with
            | VVariant (name, value) when label = name ->
                let env = evalPattern env expr value
                env
            | VVariant (name, _) ->
                raise (evalError (BadVariantPattern (label, name)))
            | _ ->
                raise (genericError (NotAVariantValue value))
        | _ -> 
            raise (genericError (InvalidPattern pattern))
    loop env pattern value

let eval expr : Value =
    evalExpr Map.empty expr
