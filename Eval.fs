module Eval

open Error
open Expr
open Pattern
open Util
open Infer

let rec evalExpr (env: Map<string, Value>) (expr: Expr) : Value =
    match expr with
    | EBool b -> VBool b
    | EInt i -> VInt i
    | EFloat f -> VFloat f
    | EFun (pattern, expr) -> VFun (pattern, expr)
    | EVar name -> 
        Map.tryFind name env
        |> Option.defaultWith (fun () ->
            raise (genericError (VariableNotFound name))
        )
    | ECall (fnExpr, argExpr) -> 
        let fnValue = evalExpr env fnExpr
        match fnValue with
        | VFun (pattern, bodyExpr) ->
            let initialEnv = env
            let argValue = evalExpr initialEnv argExpr
            let fnEnv = consumePattern env pattern argValue
            evalExpr fnEnv bodyExpr
        | _ -> raise (evalError (NotAFunction fnExpr))
    | ELet (pattern, valueExpr, bodyExpr) ->
        let value = evalExpr env valueExpr
        let env = consumePattern env pattern value
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
        match valueValue with
        | VVariant (label, value) ->
            let pattern, expr =
                cases
                |> List.tryFind (fun (caseLabel, _, _) -> caseLabel = label)
                |> Option.map (fun (_, pattern, expr) -> pattern, expr)
                |> Option.defaultWith (fun () ->
                    oDefault
                    |> Option.defaultWith (fun () ->
                        raise (evalError (MissingMatchCase valueExpr))
                    )
                )
            let initialEnv = env
            let fnEnv = consumePattern env pattern value
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
        
and consumePattern (env: Map<string, Value>) pattern (value: Value) =
    let rec loop env pattern (value: Value) =
        match pattern with
        | EVar var -> Map.add var value env
        | ERecordEmpty -> env
        | ERecordExtend (label, EVar var, record) ->
            match value with
            | VRecord fields ->
                let field = 
                    fields
                    |> Map.tryFind label
                    |> Option.defaultWith (fun () ->
                        raise (genericError (FieldNotFound label))
                    )
                let remainingFields =
                    fields
                    |> Map.remove label
                let env = Map.add var field env 
                loop env record (VRecord remainingFields)
            | _ ->
                raise (genericError (NotARecordValue value))
        | _ -> 
            raise (genericError (InvalidPattern pattern))
    loop env pattern value

let eval expr : Value =
    evalExpr Map.empty expr
