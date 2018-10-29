module Expr

open System

open Util
open System.Collections.Generic

type Name = String

type BinOp =
    | Plus
    | Minus
    | Multiply
    | Divide
    | And
    | Or
    | Equal
    | NotEqual
    | Greater
    | GreaterEqual
    | Lesser
    | LesserEqual
with
    override x.ToString () =
        match x with
        | Plus -> "Plus"
        | Minus -> "Minus"
        | Multiply -> "Multiply"
        | Divide -> "Divide"
        | And -> "And"
        | Or -> "Or"
        | Equal -> "Equal"
        | NotEqual -> "NotEqual"
        | Greater -> "Greater"
        | GreaterEqual -> "GreaterEqual"
        | Lesser -> "Lesser"
        | LesserEqual -> "LesserEqual"

type UnOp =
    | Negative

type Expr =
    | EBool of bool
    | EInt of int
    | EFloat of float
    | EVar of Name
    | ECall of Expr * Expr
    | EFun of Pattern * Expr
    | ELet of Pattern * Expr * Expr
    | ERecordSelect of Expr * Name
    | ERecordExtend of Name * Expr * Expr
    | ERecordRestrict of Expr * Name
    | ERecordEmpty
    | EVariant of Name * Expr
    | ECase of Expr * (Pattern * Expr * Guard option) list * (Name * Expr) option
    | EIfThenElse of Expr * Expr * Expr
    | EBinOp of Expr * BinOp * Expr
    | EUnOp of UnOp * Expr
with
    override x.ToString () =
        match x with
        | EBool b -> sprintf "EBool %b" b
        | EInt i -> sprintf "EInt %d" i
        | EFloat f -> sprintf "EFloat %f" f
        | EVar name -> sprintf "EVar %s" name
        | ECall (a, b) -> sprintf "ECall (%O, %O)" a b
        | EFun (a, b) -> sprintf "EFun (%O, %O)" a b
        | ELet (a, b, c) -> sprintf "ELet (%O, %O, %O)" a b c
        | ERecordSelect (a, name) -> sprintf "ERecordSelect (%O, %s)" a name
        | ERecordExtend (label, a, b) -> sprintf "ERecordExtend (%s, %O, %O)" label a b
        | ERecordRestrict (a, label) -> sprintf "ERecordRestrict (%O, %s)" a label
        | ERecordEmpty -> "ERecordEmpty"
        | EVariant (label, a) -> sprintf "EVariant (%s, %O)" label a
        | ECase (a, xs, o) -> sprintf "ECase (%O, %O, %O)" a xs o
        | EIfThenElse (a, b, c) -> sprintf "EIfThenElse (%O, %O, %O)" a b c
        | EBinOp (a, op, b) -> sprintf "EBinOp (%O, %O, %O)" a op b
        | EUnOp (op, a) -> sprintf "EUnOp (%O, %O)" op a

and Pattern = Expr
and Guard = Expr

type Id = int
type Level = int

type Ty =
    | TConst of Name
    | TBool
    | TInt
    | TFloat
    | TApp of Ty * Ty list
    | TArrow of Ty * Ty
    | TVar of Tvar ref
    | TRecord of Row
    | TVariant of Row
    | TRowEmpty
    | TRowExtend of Name * Ty * Row
with
    override x.ToString () =
        match x with
        | TConst name -> name
        | TBool -> "TBool"
        | TInt -> "TInt"
        | TFloat -> "TFloat"
        | TApp (x, xs) -> sprintf "TApp (%O, %s)" x (xs |> List.map string |> String.concat ", ")
        | TArrow (a, b) -> sprintf "TArrow (%O, %O)" a b
        | TVar a -> sprintf "TVar %O" (!a)
        | TRecord a -> sprintf "TRecord %O" a
        | TVariant a -> sprintf "TVariant %O" a
        | TRowEmpty -> "TRowEmpty"
        | TRowExtend (name, a, b) -> sprintf "TRowExtend (%s, %O, %O)" name a b

and Row = Ty

and Constraints = Set<Name>

and Tvar =
    | Unbound of Id * Level
    | UnboundRow of Id * Level * Constraints
    | Link of Ty
    | Generic of Id
    | GenericRow of Id * Constraints
with
    override x.ToString () =
        match x with
        | Unbound (id, level) -> sprintf "Unbound (%d, %d)" id level
        | UnboundRow (id, level, constraints) -> sprintf "UnboundRow (%d, %d, %O)" id level constraints
        | Link a -> sprintf "Link %O" a
        | Generic id -> sprintf "Generic %d" id
        | GenericRow (id, constraints) -> sprintf "GenericRow (%d, %O)" id constraints

type Value =
    | VBool of bool
    | VInt of int
    | VFloat of float
    | VFun of Map<Name, Value> * Pattern * Expr
    | VRecord of Map<Name, Value>
    | VVariant of Name * Value
with
    override x.ToString () =
        match x with
        | VBool b -> sprintf "VBool %b" b
        | VInt i -> sprintf "VInt %d" i
        | VFloat f -> sprintf "VFloat %f" f
        | VFun _ -> "VFun"
        | VRecord fields -> sprintf "VRecord %O" fields
        | VVariant (name, value) -> sprintf "VVariant (%s, %O)" name value

let stringOfBinOp = function
    | Plus -> "+"
    | Minus -> "-"
    | Multiply -> "*"
    | Divide -> "/"
    | And -> "&&"
    | Or -> "||"
    | Equal -> "="
    | NotEqual -> "<>"
    | Greater -> ">"
    | GreaterEqual -> ">="
    | Lesser -> "<"
    | LesserEqual -> "<="

let stringOfUnOp = function
    | Negative -> "-"

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
        | EFun (pattern, bodyExpr) ->
            let funStr = 
                sprintf "fun %s -> %s" 
                    (f false pattern)
                    (f false bodyExpr)
            if isSimple then "(" + funStr + ")" else funStr
        | ELet (pattern, valueExpr, bodyExpr) ->
            let letStr =
                sprintf "let %s = %s in %s"
                    (f false pattern)
                    (f false valueExpr)
                    (f false bodyExpr)
            if isSimple then "(" + letStr + ")" else letStr
        | ERecordEmpty -> "{}"
        | EVariant (label, value) ->
            let variantStr = ":" + label + " " + f true value 
            if isSimple then "(" + variantStr + ")" else variantStr
        | ERecordSelect (recordExpr, label) -> f true recordExpr + "." + label
        | ERecordRestrict (recordExpr, label) -> "{" + f false recordExpr + " - " + label + "}"
        | ERecordExtend (name, valueExpr, restExpr) ->
            let rec g str = function
                | ERecordEmpty -> str
                | ERecordExtend (label, valueExpr, restExpr) ->
                    g (str + ", " + label + " = " + f false valueExpr) restExpr
                | otherExpr -> str + " | " + f false otherExpr
            "{" + g (name + " = " + f false valueExpr) restExpr + "}"
        | ECase (expr, cases, oDefault) ->
            let caseStrList = 
                cases
                |> List.map (fun (pattern, expr, oGuard) ->
                    let guard =
                        match oGuard with
                        | None -> ""
                        | Some guard -> sprintf " when %O" (f false guard)
                    f false pattern + guard + " -> " + f false expr
                )
                |> String.concat ", "
            let defaultStr =
                match oDefault with
                | None -> ""
                | Some (name, expr) -> sprintf "| %s -> %s" name (f false expr)
            "match " + f false expr + " { " + caseStrList + defaultStr + " } "
        | EIfThenElse (ifExpr, thenExpr, elseExpr) ->
            let a = f false ifExpr
            let b = f false thenExpr
            let c = f false elseExpr
            sprintf "if %s then %s else %s" a b c
        | EBinOp (a, op, b) ->
            let a = f false a
            let op = stringOfBinOp op
            let b = f false b
            sprintf "%s %s %s" a op b
        | EUnOp (op, a) ->
            let a = f false a
            let op = stringOfUnOp op
            sprintf "%s%s" op a
    f false x

type Entry = {
    Name: Name
    Constraints: Set<Name>
}
with
    static member Simple name = { Name = name; Constraints = Set.empty }
    static member Row name constraints = { Name = name; Constraints = constraints }

let stringOfTy (x: Ty) : string =
    let mutable idNameMap = Map.empty<Id, Entry>
    let mutable count = 0
    let mutable rowCount = 0
    let nextName () =
        let i = count
        count <- i + 1
        let name = char(97 + i)
        string name
    let nextRowName () =
        let i = rowCount
        rowCount <- i + 1
        let name = if i = 0 then "r" else sprintf "r%d" i
        string name
    let genericName id =
        idNameMap
        |> Map.tryFind id
        |> Option.map (fun entry -> entry.Name)
        |> Option.defaultWith (fun () ->
            let name = nextName ()
            idNameMap <-
                idNameMap
                |> Map.add id (Entry.Simple name)
            name
        )
    let genericRowName id constraints =
        idNameMap
        |> Map.tryFind id
        |> Option.map (fun entry -> entry.Name)
        |> Option.defaultWith (fun () ->
            let name = nextRowName ()
            idNameMap <-
                idNameMap
                |> Map.add id (Entry.Row name constraints)
            name
        )
    let rec f isSimple = function
        | TConst name -> name
        | TBool -> "bool"
        | TInt -> "int"
        | TFloat -> "float"
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
            let name = genericName id
            name
        | TVar {contents = GenericRow (id, constraints)} ->
            let name = genericRowName id constraints
            name
        | TVar {contents = Unbound(id, _)} -> "_" + string id
        | TVar {contents = UnboundRow(id, _, _)} -> "_" + string id
        | TVar {contents = Link ty} -> f isSimple ty
        | TRecord rowTy -> "{" + f false rowTy + "}"
        | TVariant rowTy -> "<" + f false rowTy + ">"
        | TRowEmpty -> ""
        | TRowExtend (label, ty, rowTy) ->
            let rec g xs = function
                | TRowEmpty -> xs, ""
                | TRowExtend (label, ty, rowTy) ->
                    g ((label, f false ty) :: xs) rowTy
                | TVar {contents = Link ty} -> g xs ty
                | otherTy -> xs, sprintf " | %s" (f false otherTy)
            let labels, rest = g [label, f false ty] rowTy
            let labels =
                labels
                |> List.sortBy fst
                |> List.map (fun (label, ty) -> sprintf "%s : %s" label ty)
                |> String.concat ", "
            labels + rest
    let tyStr = f false x
    if count > 0 || rowCount > 0 then
        let varNames = 
            ([], idNameMap)
            ||> Map.fold (fun acc _ value -> value :: acc)
        let args =
            varNames
            |> List.map (fun entry -> entry.Name)
            |> List.sort
            |> String.concat " "
        let constraints =
            let constraints =
                varNames
                |> List.sortBy (fun entry -> entry.Name)
                |> List.choose (fun entry ->
                    if Set.isEmpty entry.Constraints
                    then None
                    else 
                        Set.fold (fun state constraint_ -> state + "\\" + constraint_) entry.Name entry.Constraints
                        |> Some
                )
            if List.isEmpty constraints 
            then " => "
            else (sprintf ". (%s) => " (String.concat ", " constraints))
        "forall " + args + constraints + tyStr
    else
        tyStr

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
            |> List.sortBy fst
            |> List.map (fun (label, value) -> sprintf "%s : %s" label (stringOfValue value))
            |> String.concat ", "
            |> sprintf "{ %s }"
        | VVariant (label, value) -> sprintf ":%s %s" label (stringOfValue value)
    f false value

let tryMakeVariantCases cases =
    let transformed =
        cases
        |> List.choose (fun case ->
            match case with
            | EVariant (name, pattern), expr, guard -> 
                Some (name, pattern, expr, guard)
            | _ -> None
        )
    if List.length transformed = List.length cases then
        Some transformed
    else
        None
