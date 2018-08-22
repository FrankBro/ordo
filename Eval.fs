module Eval

open Expr

type Value =
    | VBool of bool
    | VInt of int
    | VFloat of float
    | VFun
    | VRecord of Map<Name, Value>

let rec evalExpr env (expr: Expr) : Expr =
    match expr with
    | EBool _ 
    | EInt _
    | EFloat _
    | EFun _ -> expr
    | EVar name -> evalExpr env (Map.find name env)
    | ECall (fnExpr, argExpr) -> evalCall env fnExpr argExpr
    | ELet (name, valueExpr, bodyExpr) ->
        let value = evalExpr env valueExpr
        let env = Map.add name value env
        evalExpr env bodyExpr

and evalCall env (fnExpr: Expr) (argExpr: Expr) =
    let evaled = evalExpr env fnExpr
    match evaled with
    | EFun (argName, bodyExpr) ->
        let initialEnv = env
        let argValue = evalExpr initialEnv argExpr
        let fnEnv = Map.add argName argValue env
        evalExpr fnEnv bodyExpr
    | _ -> failwithf "fn_value was not a fun, it was a %O" fnExpr

let eval expr : Value =
    let result = evalExpr Map.empty expr
    match result with
    | EBool b -> VBool b
    | EInt i -> VInt i
    | EFloat f -> VFloat f
    | EFun _ -> VFun 

let stringOfValue value =
    match value with
    | VBool b -> string b
    | VInt i -> string i
    | VFloat f -> string f
    | VFun -> "<Lambda>"
