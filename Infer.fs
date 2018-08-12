module Infer

open Expr

let currentId = ref 0

let nextId () =
    let id = !currentId
    currentId := id + 1
    id

let resetId () = currentId := 0

let newVar level = TVar (ref (Unbound(nextId (), level)))
let newGenVar () = TVar (ref (Generic(nextId ())))

exception Error of string
let error msg = raise (Error msg)

let occursCheckAdjustLevels tvarId tvarLevel ty =
    let rec f = function
        | TVar {contents = Link ty} -> f ty
        | TVar {contents = Generic _ } -> assert false
        | TVar ({contents = Unbound (otherId, otherLevel)} as otherTvar) ->
            if otherId = tvarId then
                error "recursive types"
            else
                if otherLevel > tvarLevel then
                    otherTvar := Unbound(otherId, tvarLevel)
                else
                    ()
        | TApp (ty, tyArgList) ->
            f ty
            List.iter f tyArgList
        | TArrow (paramTyList, returnTy) ->
            List.iter f paramTyList
            f returnTy
        | TConst _ -> ()
    f ty

let rec unify ty1 ty2 =
    if ty1 = ty2 then () else
    match ty1, ty2 with
    | TConst name1, TConst name2 when name1 = name2 -> ()
    | TApp (ty1, tyArgList1), TApp (ty2, tyArgList2) ->
        unify ty1 ty2
        List.iter2 unify tyArgList1 tyArgList2
    | TArrow (paramTyList1, returnTy1), TArrow (paramTyList2, returnTy2) ->
        List.iter2 unify paramTyList1 paramTyList2
        unify returnTy1 returnTy2
    | TVar {contents = Link ty1}, ty2
    | ty1, TVar {contents = Link ty2} -> unify ty1 ty2
    | TVar {contents = Unbound(id1, _)}, TVar {contents = Unbound(id2, _)} when id1 = id2 ->
        assert false // There is only a single instance of a particular type variable
    | TVar ({contents = Unbound(id, level)} as tvar), ty
    | ty, TVar ({contents = Unbound(id, level)} as tvar) ->
        occursCheckAdjustLevels id level ty
        tvar := Link ty
    | _, _ -> 
        sprintf "cannot unify types %O and %O" ty1 ty2
        |> error 

let rec generalize level = function
    | TVar {contents = Unbound(id, otherLevel)} when otherLevel > level ->  
        TVar (ref (Generic id))
    | TApp (ty, tyArgList) ->
        TApp (generalize level ty, List.map (generalize level) tyArgList)
    | TArrow (paramTyList, returnTy) ->
        TArrow (List.map (generalize level) paramTyList, generalize level returnTy)
    | TVar {contents = Link ty} -> generalize level ty
    | TVar {contents = Generic _ }
    | TVar {contents = Unbound _ }
    | TConst _ as ty -> ty

let instantiate level ty =
    let mutable idVarMap = Map.empty
    let rec f ty =
        match ty with
        | TConst _ -> ty
        | TVar {contents = Link ty} -> f ty
        | TVar {contents = Generic id} ->
            idVarMap
            |> Map.tryFind id
            |> Option.defaultWith (fun () ->
                let var = newVar level
                idVarMap <- Map.add id var idVarMap
                var
            )
        | TVar {contents = Unbound _} -> ty
        | TApp (ty, tyArgList) ->
            TApp (f ty, List.map f tyArgList)
        | TArrow (paramTyList, returnTy) ->
            TArrow (List.map f paramTyList, f returnTy)
    f ty

let rec matchFunTy numParams = function
    | TArrow (paramTyList, returnTy) ->
        if List.length paramTyList <> numParams then
            error "unexpected number of arguments"
        else
            paramTyList, returnTy
    | TVar {contents = Link ty} -> matchFunTy numParams ty
    | TVar ({contents = Unbound(id, level)} as tvar) ->
        let paramTyList =
            let rec f = function
                | 0 -> []
                | n -> newVar level :: f (n - 1)
            f numParams
        let returnTy = newVar level
        tvar := Link (TArrow(paramTyList, returnTy))
        paramTyList, returnTy
    | _ -> error "expected a function"

let rec infer env level = function
    | EValue (VBool _) -> TConst "bool"
    | EValue (VInt _) -> TConst "int"
    | EValue (VFloat _) -> TConst "float"
    | EValue (VFun (paramList, bodyExpr)) ->
        let paramTyList = List.map (fun _ -> newVar level) paramList
        let fnEnv = 
            List.foldBack2 (fun paramName paramTy env ->
                Map.add paramName paramTy env
            ) paramList paramTyList env
        let returnTy = infer fnEnv level bodyExpr
        TArrow (paramTyList, returnTy)
    | EVar name ->
        env
        |> Map.tryFind name
        |> Option.map (instantiate level)
        |> Option.defaultWith (fun () ->
            sprintf "variable %s not found" name
            |> error
        )
    | ELet (varName, valueExpr, bodyExpr) ->
        let varTy = infer env (level + 1) valueExpr
        let generalizedTy = generalize level varTy
        infer (Map.add varName generalizedTy env) level bodyExpr
    | ECall (fnExpr, argList) ->
        let paramTyList, returnTy =
            matchFunTy (List.length argList) (infer env level fnExpr)
        (paramTyList, argList)
        ||> List.iter2 (fun paramTy argExpr -> unify paramTy (infer env level argExpr))
        returnTy
    