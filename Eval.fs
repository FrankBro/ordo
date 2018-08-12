module Eval

open Expr

let rec evalExpr env (expr: Expr) : Value =
    match expr with
    | EValue v -> v
    | EVar name -> evalExpr env (EValue (Map.find name env))
    | ECall (fnExpr, argExprs) -> evalCall env fnExpr argExprs
    | ELet (name, valueExpr, bodyExpr) ->
        let value = evalExpr env valueExpr
        let env = Map.add name value env
        evalExpr env bodyExpr

and evalCall env (fnExpr: Expr) (argExprs: Expr list) : Value =
    let fnValue =
        match fnExpr with
        | EVar name -> Map.find name env
        | _ -> failwith "Call first arg is not a variable"
    match fnValue with
    | VFun (argNames, bodyExpr) ->
        let initialEnv = env
        let fnEnv =
            List.fold2 (fun env argName argExpr ->
                let argValue = evalExpr initialEnv argExpr
                Map.add argName argValue env
            ) env argNames argExprs
        evalExpr fnEnv bodyExpr
    | _ -> failwith "fn_value was not a fun"

let eval expr : Value =
    evalExpr Map.empty expr
