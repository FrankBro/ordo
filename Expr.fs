module Expr
open System
open System.Data.SqlTypes
open System.Reflection.Metadata.Ecma335

type Name = String

and Expr =
    | EVar of Name
    | EFun of Name * Expr
    | ECall of Expr * Expr
    | ELet of Name * Expr * Expr
with
    override x.ToString () =
        let rec f isSimple = function
            | EVar name -> name
            | EFun (param, bodyExpr) ->
                let funStr = 
                    sprintf "fun %s -> %s" 
                        param
                        (string bodyExpr)
                if isSimple then "(" + funStr + ")" else funStr
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

and Tvar =
    | Unbound of Id * Level
    | Link of Ty
    | Generic of Id

