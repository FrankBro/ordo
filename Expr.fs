module Expr

open System

open Util

type Name = String

type Expr =
    | EBool of bool
    | EInt of int
    | EFloat of float
    | EVar of Name
    | ECall of Expr * Expr
    | EFun of Name * Expr
    | ELet of Name * Expr * Expr
    | ERecordSelect of Expr * Name
    | ERecordExtend of Name * Expr * Expr
    | ERecordRestrict of Expr * Name
    | ERecordEmpty
    | EVariant of Name * Expr
    // | ECase of Expr * (Name * Name * Expr) list * (Name * Expr) option

type Id = int
type Level = int

type Ty =
    | TConst of Name
    | TApp of Ty * Ty list
    | TArrow of Ty * Ty
    | TVar of Tvar ref
    | TRecord of Row
    | TVariant of Row
    | TRowEmpty
    | TRowExtend of Name * Ty * Row

and Row = Ty

and Tvar =
    | Unbound of Id * Level
    | Link of Ty
    | Generic of Id

let stringOfExpr (x: Expr) : string =
    let rec f isSimple = function
        | EBool bool -> sprintf "%b" bool
        | EInt int -> sprintf "%d" int
        | EFloat float -> sprintf "%f" float
        | EVar name -> name
        | ECall (fnExpr, argExpr) ->
            let fnStr = f true fnExpr
            let argStr = f false argExpr
            sprintf "%s %s" fnStr argStr
        | EFun (param, bodyExpr) ->
            let funStr = 
                sprintf "fun %s -> %s" 
                    param
                    (f false bodyExpr)
            if isSimple then "(" + funStr + ")" else funStr
        | ELet (varName, valueExpr, bodyExpr) ->
            let letStr =
                sprintf "let %s = %s in %s"
                    varName
                    (f false valueExpr)
                    (f false bodyExpr)
            if isSimple then "(" + letStr + ")" else letStr
        | ERecordEmpty -> "{}"
        | EVariant (label, expr) -> sprintf ":%s %s" label (f false expr)
        | ERecordSelect (recordExpr, label) -> f true recordExpr + "." + label
        | ERecordRestrict (recordExpr, label) -> "{" + f false recordExpr + " - " + label + "}"
        | ERecordExtend (name, valueExpr, restExpr) ->
            let rec g str = function
                | ERecordEmpty -> str
                | ERecordExtend (label, valueExpr, restExpr) ->
                    g (str + ", " + label + " = " + f false valueExpr) restExpr
                | otherExpr -> str + " | " + f false otherExpr
            "{" + g (name + " = " + f false valueExpr) restExpr + "}"
        // | EVariant (label, value) ->
        //     let variantStr = ":" + label + " " + f true value 
        //     if isSimple then "(" + variantStr + ")" else variantStr
        // | ECase (expr, cases, maybeDefaultCase) ->
        //     let caseStrList = 
        //         cases
        //         |> List.map (fun (label, varName, expr) ->
        //             "| :" + label + " " + varName + " -> " + f false expr
        //         )
        //     let allCasesStr =
        //         match caseStrList, maybeDefaultCase with
        //         | [], Some (varName, expr) -> varName + " -> " + f false expr
        //         | casesStrList, None -> String.concat "" caseStrList
        //         | casesStrList, Some (varName, expr) ->
        //             String.concat "" casesStrList + " | " + varName + " -> " + f false expr
        //     "match " + f false expr + " { " + allCasesStr + " } "
    f false x

let stringOfTy (x: Ty) : string =
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
        | TRecord rowTy -> "{" + f false rowTy + "}"
        | TVariant rowTy -> "<" + f false rowTy + ">"
        // | TVariant rowTy -> "[" + f false rowTy + "]"
        | TRowEmpty -> ""
        | TRowExtend (label, ty, rowTy) ->
            let rec g str = function
                | TRowEmpty -> str
                | TRowExtend (label, ty, rowTy) ->
                    g (str + ", " + label + " : " + f false ty) rowTy
                | TVar {contents = Link ty} -> g str ty
                | otherTy -> str + " | " + f false otherTy
            g (label + " : " + f false ty) rowTy
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
