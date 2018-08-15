module Expr

open System

open Util

type Name = String

type Value =
    | VBool of bool
    | VInt of int
    | VFloat of float
    | VFun of Name * Expr
with
    override x.ToString () =
        let rec f isSimple = function
            | VBool bool -> sprintf "%b" bool
            | VInt int -> sprintf "%d" int
            | VFloat float -> sprintf "%f" float
            | VFun (param, bodyExpr) ->
                let funStr = 
                    sprintf "fun %s -> %s" 
                        param
                        (string bodyExpr)
                if isSimple then "(" + funStr + ")" else funStr
        f false x
    
    static member ToStringRaw (x: Value) =
        match x with
        | VBool b -> sprintf "VBool %b" b
        | VInt i -> sprintf "VInt %d" i
        | VFloat f -> sprintf "VFloat %f" f
        | VFun (name, expr) -> sprintf "VFun (%s, %s)" name (Expr.ToStringRaw expr)

and Expr =
    | EValue of Value
    | EVar of Name
    | ECall of Expr * Expr
    | ELet of Name * Expr * Expr
with
    override x.ToString () =
        let rec f isSimple = function
            | EValue value -> string value
            | EVar name -> name
            | ECall (fnExpr, argExpr) ->
                let fnStr = f true fnExpr
                let argStr = f false argExpr
                sprintf "%s %s" fnStr argStr
            | ELet (varName, valueExpr, bodyExpr) ->
                let letStr =
                    sprintf "let %s = %s in %s"
                        varName
                        (f false valueExpr)
                        (f false bodyExpr)
                if isSimple then "(" + letStr + ")" else letStr
        f false x

    static member ToStringRaw (x: Expr) =
        match x with
        | EValue v -> sprintf "EValue %s" (Value.ToStringRaw v)
        | EVar name -> sprintf "EVar %s" name
        | ECall (a, b) -> sprintf "ECall (%s, %s)" (Expr.ToStringRaw a) (Expr.ToStringRaw b)
        | ELet (name, value, body) -> sprintf "ELet (%s, %s, %s" name (Expr.ToStringRaw value) (Expr.ToStringRaw body)

type Id = int
type Level = int

type Ty =
    | TConst of Name
    | TApp of Ty * Ty list
    | TArrow of Ty * Ty
    | TVar of Tvar ref
with
    override x.ToString () =
        let mutable idNameMap = Map.empty
        let mutable count = 0
        let nextName () =
            let i = count
            count <- i + 1
            let name = char(97 + i)
            string name
        let rec f isSimple = function
            | TConst name -> name
            | TApp (ty, tyArgList) ->
                tyArgList
                |> List.map (f false)
                |> String.concat ", "
                |> sprintf "%s[%s]" (f true ty)
            | TArrow (paramTy, returnTy) ->
                let arrowTyStr =
                    let paramTyStr = f true paramTy
                    let returnTyStr = f false returnTy
                    sprintf "%s -> %s" paramTyStr returnTyStr
                if isSimple then "(" + arrowTyStr + ")" else arrowTyStr
            | TVar {contents = Generic id} ->
                idNameMap
                |> Map.tryFind id
                |> Option.defaultWith (fun () ->
                    let name = nextName ()
                    idNameMap <-
                        idNameMap
                        |> Map.add id name
                    name
                )
            | TVar {contents = Unbound(id, _)} -> "_" + string id
            | TVar {contents = Link ty} -> f isSimple ty
        let tyStr = f false x
        if count > 0 then
            let varNames = 
                ([], idNameMap)
                ||> Map.fold (fun acc _ value -> value :: acc)
            let args =
                varNames
                |> List.sort
                |> String.concat " "
            "forall[" + args + "] " + tyStr
        else
            tyStr

    static member ToStringRaw (x: Ty) =
        match x with
        | TConst name -> sprintf "TConst %s" name
        | TApp (ty, args) ->
            let argsString =
                args
                |> List.map Ty.ToStringRaw
                |> String.concat ", "
            sprintf "TApp (%s, [%s])" (Ty.ToStringRaw ty) argsString
        | TArrow (a, b) -> sprintf "TArrow (%s, %s)" (Ty.ToStringRaw a) (Ty.ToStringRaw b)
        | TVar {contents = Unbound (id, level)} -> sprintf "TVar (Unbound (%d, %d))" id level
        | TVar {contents = Link ty} -> sprintf "TVar (Link %s)" (Ty.ToStringRaw ty)
        | TVar {contents = Generic id} -> sprintf "TVar (Generic %d)" id

and Tvar =
    | Unbound of Id * Level
    | Link of Ty
    | Generic of Id
