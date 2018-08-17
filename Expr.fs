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
    | ERecordSelect of Expr * Name
    | ERecordExtend of Map<String, Expr list> * Expr
    | ERecordRestrict of Expr * Name
    | ERecordEmpty
    | EVariant of Name * Expr
    | ECase of Expr * (Name * Name * Expr) list * (Name * Expr) option
with
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
    | TRecord of Row
    | TVariant of Row
    | TRowEmpty
    | TRowExtend of Map<string, Ty list> * Row
with
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

and Row = Ty

and Tvar =
    | Unbound of Id * Level
    | Link of Ty
    | Generic of Id

let rec realTy = function
    | TVar {contents = Link ty} -> realTy ty
    | ty -> ty

let mergeLabels labels1 labels2 =
    let keys =
        List.append
            (Map.toList labels1 |> List.map fst)
            (Map.toList labels2 |> List.map fst)
        |> Set.ofList
    (Map.empty, keys)
    ||> Set.fold (fun state key ->
        match Map.tryFind key labels1, Map.tryFind key labels2 with
        | None, None -> state
        | Some ty1, None ->
            state
            |> Map.add key ty1
        | None, Some ty2 ->
            state
            |> Map.add key ty2
        | Some ty1, Some ty2 ->
            state
            |> Map.add key (List.append ty1 ty2)
    )

let rec matchRowTy = function
    | TRowExtend (labelTyMap, restTy) ->
        match matchRowTy restTy with
        | restLabelTyMap, restTy when Map.isEmpty restLabelTyMap ->
            (labelTyMap, restTy)
        | restLabelTyMap, restTy ->
            (mergeLabels labelTyMap restLabelTyMap, restTy)
    | TVar {contents = Link ty} -> matchRowTy ty
    | TVar _ as var -> Map.empty, var
    | TRowEmpty -> Map.empty, TRowEmpty
    | ty -> raise (Failure "not a row")

let addDistinctLabels labelElMap labelElList =
    (labelElMap, labelElList)
    ||> List.fold (fun labelElMap (label, el) ->
        assert (not (Map.containsKey label labelElMap))
        Map.add label el labelElMap
    )

let labelMapFromList labelElList =
    addDistinctLabels Map.empty labelElList

let stringOfValue (x: Value) : string =
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
    
let stringOfExpr (x: Expr) : string =
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
        | ERecordEmpty -> "{}"
        | ERecordSelect (recordExpr, label) -> f true recordExpr + "." + label
        | ERecordRestrict (recordExpr, label) -> "{" + f false recordExpr + " - " + label + "}"
        | ERecordExtend (labelExprMap, restExpr) ->
            let labelExprStr =
                labelExprMap
                |> Map.toList
                |> List.map (fun (label, exprList) ->
                    exprList
                    |> List.map (fun expr -> label + " = " + f false expr)
                    |> String.concat ", "
                )
                |> String.concat ", "
            let restExprStr =
                match restExpr with
                | ERecordEmpty -> ""
                | expr -> " | " + f false expr
            "{" + labelExprStr + restExprStr + "}"
        | EVariant (label, value) ->
            let variantStr = ":" + label + " " + f true value 
            if isSimple then "(" + variantStr + ")" else variantStr
        | ECase (expr, cases, maybeDefaultCase) ->
            let caseStrList = 
                cases
                |> List.map (fun (label, varName, expr) ->
                    "| :" + label + " " + varName + " -> " + f false expr
                )
            let allCasesStr =
                match caseStrList, maybeDefaultCase with
                | [], Some (varName, expr) -> varName + " -> " + f false expr
                | casesStrList, None -> String.concat "" caseStrList
                | casesStrList, Some (varName, expr) ->
                    String.concat "" casesStrList + " | " + varName + " -> " + f false expr
            "match " + f false expr + " { " + allCasesStr + " } "
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
        | TVariant rowTy -> "[" + f false rowTy + "]"
        | TRowEmpty -> ""
        | TRowExtend _ as rowTy ->
            let labelTyMap, restTy = matchRowTy rowTy
            let labelTyStr =
                labelTyMap
                |> Map.toList
                |> List.map (fun (label, tyList) ->
                    tyList
                    |> List.map (fun ty -> label + " : " + f false ty)
                    |> String.concat ", "
                )
                |> String.concat ", "
            let restTyStr =
                match realTy restTy with
                | TRowEmpty -> ""
                | TRowExtend _ -> failwith ""
                | otherTy -> " | " + f false otherTy
            labelTyStr + restTyStr
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
