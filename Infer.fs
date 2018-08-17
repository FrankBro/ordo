module Infer

open Expr
open Util

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
        | TRecord row -> f row
        | TVariant row -> f row
        | TRowExtend (labelTyMap, restTy) ->
            labelTyMap
            |> Map.iter (fun _ -> List.iter f)
            f restTy
        | TConst _ | TRowEmpty -> ()
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
    | TRecord row1, TRecord row2 -> unify row1 row2
    | TVariant row1, TVariant row2 -> unify row1 row2
    | TRowEmpty, TRowEmpty -> ()
    | (TRowExtend _ as row1), (TRowExtend _ as row2) -> unifyRows row1 row2
    | TRowEmpty, TRowExtend (labelTyMap, _)
    | TRowExtend (labelTyMap, _), TRowEmpty ->
        let label = Map.pick (fun key _ -> Some key) labelTyMap
        error ("row does not contain label " + label)
    | _, _ -> 
        sprintf "cannot unify types %O and %O" ty1 ty2
        |> error 

and unifyRows (row1: Ty) (row2: Ty) =
    let labelTyMap1, restTy1 = matchRowTy row1
    let labelTyMap2, restTy2 = matchRowTy row2

    let rec unifyTypes tyList1 tyList2 =
        match tyList1, tyList2 with
        | ty1 :: rest1, ty2 :: rest2 -> 
            unify ty1 ty2
            unifyTypes rest1 rest2
        | _ -> tyList1, tyList2
    
    let rec unifyLabels missing1 missing2 labels1 labels2 =
        match labels1, labels2 with
        | [], [] -> missing1, missing2
        | [], _ -> addDistinctLabels missing1 labels2, missing2
        | _, [] -> missing1, addDistinctLabels missing2 labels1
        | (label1, tyList1) :: rest1, (label2, tyList2) :: rest2 ->
            match compare label1 label2 with
            | 0 ->
                let missing1, missing2 = 
                    match unifyTypes tyList1 tyList2 with
                    | [], [] -> missing1, missing2
                    | tyList1, [] -> missing1, Map.add label1 tyList1 missing2
                    | [], tyList2 -> Map.add label2 tyList2 missing1, missing2
                    | _ -> failwith ""
                unifyLabels missing1 missing2 rest1 rest2
            | x when x < 0 ->
                unifyLabels missing1 (Map.add label1 tyList1 missing2) rest1 labels2
            | x ->
                unifyLabels (Map.add label2 tyList2 missing1) missing2 labels1 rest2
    let missing1, missing2 =
        unifyLabels Map.empty Map.empty
            (Map.toList labelTyMap1)
            (Map.toList labelTyMap2)
    match Map.isEmpty missing1, Map.isEmpty missing2 with
    | true, true -> unify restTy1 restTy2
    | true, false -> unify restTy2 (TRowExtend (missing2, restTy1))
    | false, true -> unify restTy1 (TRowExtend (missing1, restTy2))
    | false, false ->
        match restTy1 with
        | TRowEmpty ->
            // will result in an error
            unify restTy1 (TRowExtend (missing1, newVar 0))
        | TVar ({contents = Unbound (_, level)} as tvarRef) ->
            let newRestRowVar = newVar level
            unify restTy2 (TRowExtend (missing2, newRestRowVar))
            match !tvarRef with
            | Link _ -> error "recursive row types"
            | _ -> ()
            unify restTy1 (TRowExtend (missing1, newRestRowVar))
        | _ -> failwith ""

let rec generalizeTy level = function
    | TVar {contents = Unbound(id, otherLevel)} when otherLevel > level ->  
        TVar (ref (Generic id))
    | TApp (ty, tyArgList) ->
        TApp (generalizeTy level ty, List.map (generalizeTy level) tyArgList)
    | TArrow (paramTy, returnTy) ->
        TArrow (generalizeTy level paramTy, generalizeTy level returnTy)
    | TVar {contents = Link ty} -> generalizeTy level ty
    | TRecord row -> TRecord (generalizeTy level row)
    | TVariant row -> TVariant (generalizeTy level row)
    | TRowExtend (labelTyMap, restTy) ->
        TRowExtend (Map.map (fun _ -> List.map (generalizeTy level)) labelTyMap, generalizeTy level restTy)
    | TVar {contents = Generic _ }
    | TVar {contents = Unbound _ }
    | TConst _
    | TRowEmpty as ty -> ty

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
        | TRecord row -> TRecord (f row)
        | TVariant row -> TVariant (f row)
        | TRowEmpty -> ty
        | TRowExtend (labelTyMap, restTy) ->
            TRowExtend (Map.map (fun _ -> List.map f) labelTyMap, f restTy)
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
    | ERecordEmpty -> TRecord TRowEmpty
    | ERecordSelect (recordExpr, label) ->
        let restRowTy = newVar level
        let fieldTy = newVar level
        let paramTy = TRecord (TRowExtend(Map.singleton label [fieldTy], restRowTy))
        let returnTy = fieldTy
        unify paramTy (inferExpr env level recordExpr)
        returnTy
    | ERecordRestrict (recordExpr, label) ->
        let restRowTy = newVar level
        let fieldTy = newVar level
        let paramTy = TRecord (TRowExtend(Map.singleton label [fieldTy], restRowTy))
        let returnTy = TRecord restRowTy
        unify paramTy (inferExpr env level recordExpr)
        returnTy
    | ERecordExtend (labelExprMap, recordExpr) ->
        let labelTyMap =
            labelExprMap
            |> Map.map (fun _ ->
                List.map (fun argExpr -> inferExpr env level argExpr)
            )
        let restRowTy = newVar level
        unify (TRecord restRowTy) (inferExpr env level recordExpr)
        TRecord (TRowExtend (labelTyMap, restRowTy))
    | EVariant (label, expr) ->
        let restRowTy = newVar level
        let variantTy = newVar level
        let param_ty = variantTy in
        let return_ty = TVariant (TRowExtend(Map.singleton label [variantTy], restRowTy))
        unify param_ty (inferExpr env level expr)
        return_ty
    | ECase (expr, cases, None) ->
        let returnTy = newVar level
        let exprTy = inferExpr env level expr
        let casesRow = inferCases env level returnTy TRowEmpty cases
        unify exprTy (TVariant casesRow)
        returnTy
    | ECase (expr, cases, Some (defaultVarName, defaultExpr)) ->
        let defaultVariantTy = newVar level
        let returnTy = inferExpr (Map.add defaultVarName (TVariant defaultVariantTy) env) level defaultExpr
        let exprTy = inferExpr env level expr
        let casesRow = inferCases env level returnTy defaultVariantTy cases
        unify exprTy (TVariant casesRow)
        returnTy

and inferCases env level returnTy restRowTy cases =
    match cases with
    | [] -> restRowTy
    | (label, varName, expr) :: otherCases ->
        let variantTy = newVar level
        unify returnTy (inferExpr (Map.add varName variantTy env) level expr)
        let otherCasesRow = inferCases env level returnTy restRowTy otherCases
        TRowExtend (Map.singleton label [variantTy], otherCasesRow)
   
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
    resetId ()
    inferExpr Map.empty 0 expr
    |> generalize
