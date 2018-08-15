module Eval

open Expr

let rec evalExpr env (expr: Expr) : Value =
    match expr with
    | EValue v -> v
    | EVar name -> evalExpr env (EValue (Map.find name env))
    | ECall (fnExpr, argExpr) -> evalCall env fnExpr argExpr
    | ELet (name, valueExpr, bodyExpr) ->
        let value = evalExpr env valueExpr
        let env = Map.add name value env
        evalExpr env bodyExpr

and evalCall env (fnExpr: Expr) (argExpr: Expr) : Value =
    let fnValue = evalExpr env fnExpr
    match fnValue with
    | VFun (argName, bodyExpr) ->
        let initialEnv = env
        let argValue = evalExpr initialEnv argExpr
        let fnEnv = Map.add argName argValue env
        evalExpr fnEnv bodyExpr
    | _ -> failwith "fn_value was not a fun"

let eval expr : Value =
    evalExpr Map.empty expr
