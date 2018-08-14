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
        | TArrow (paramTy, returnTy) ->
            f paramTy
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
    | TArrow (paramTy1, returnTy1), TArrow (paramTy2, returnTy2) ->
        unify paramTy1 paramTy2
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

let rec generalizeTy level = function
    | TVar {contents = Unbound(id, otherLevel)} when otherLevel > level ->  
        TVar (ref (Generic id))
    | TApp (ty, tyArgList) ->
        TApp (generalizeTy level ty, List.map (generalizeTy level) tyArgList)
    | TArrow (paramTy, returnTy) ->
        TArrow (generalizeTy level paramTy, generalizeTy level returnTy)
    | TVar {contents = Link ty} -> generalizeTy level ty
    | TVar {contents = Generic _ }
    | TVar {contents = Unbound _ }
    | TConst _ as ty -> ty

let generalize ty =
    generalizeTy (-1) ty

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
        | TArrow (paramTy, returnTy) ->
            TArrow (f paramTy, f returnTy)
    f ty

let rec matchFunTy = function
    | TArrow (paramTy, returnTy) -> paramTy, returnTy
    | TVar {contents = Link ty} -> matchFunTy ty
    | TVar ({contents = Unbound(id, level)} as tvar) ->
        let paramTy = newVar level
        let returnTy = newVar level
        tvar := Link (TArrow(paramTy, returnTy))
        paramTy, returnTy
    | _ -> error "expected a function"

let rec inferExpr env level = function
    | EValue value -> inferValue env level value
    | EVar name ->
        env
        |> Map.tryFind name
        |> Option.map (instantiate level)
        |> Option.defaultWith (fun () ->
            sprintf "variable %s not found" name
            |> error
        )
    | ELet (varName, valueExpr, bodyExpr) ->
        let varTy = inferExpr env (level + 1) valueExpr
        let generalizedTy = generalizeTy level varTy
        inferExpr (Map.add varName generalizedTy env) level bodyExpr
    | ECall (fnExpr, argExpr) ->
        let paramTy, returnTy =
            matchFunTy (inferExpr env level fnExpr)
        unify paramTy (inferExpr env level argExpr)
        returnTy
    
and inferValue env level = function
    | VBool _ -> TConst "bool"
    | VInt _ -> TConst "int"
    | VFloat _ -> TConst "float"
    | VFun (param, bodyExpr) ->
        let paramTy = newVar level
        let fnEnv = Map.add param paramTy env
        let returnTy = inferExpr fnEnv level bodyExpr
        TArrow (paramTy, returnTy)

let infer expr =
    inferExpr Map.empty 0 expr
