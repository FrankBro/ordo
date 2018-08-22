module Infer

open Error
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

let occursCheckAdjustLevels tvarId tvarLevel ty =
    let rec f = function
        | TVar {contents = Link ty} -> f ty
        | TVar {contents = Generic _ } -> assert false
        | TVar ({contents = Unbound (otherId, otherLevel)} as otherTvar) ->
            if otherId = tvarId then
                raise (ErrorException RecursiveTypes)
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
        // | TVariant row -> f row
        | TRowExtend (label, fieldTy, row) ->
            f fieldTy
            f row
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
    // | TVariant row1, TVariant row2 -> unify row1 row2
    | TRowEmpty, TRowEmpty -> ()
    | TRowExtend (label1, fieldTy1, restRow1), (TRowExtend _ as row2) -> 
        let restRow1TVarRefOption =
            match restRow1 with
            | TVar ({contents = Unbound _} as tvarRef) -> Some tvarRef
            | _ -> None
        let restRow2 = rewriteRow row2 label1 fieldTy1
        match restRow1TVarRefOption with
        | Some {contents = Link _} -> raise (ErrorException RecursiveRowTypes)
        | _ -> ()
        unify restRow1 restRow2
    | _, _ -> 
        raise (ErrorException (UnifyFail (ty1, ty2)))

and rewriteRow (row2: Ty) label1 fieldTy1 =
    match row2 with
    | TRowEmpty -> raise (ErrorException (RowMissingLabel label1))
    | TRowExtend (label2, fieldTy2, restRow2) when label2 = label1 ->
        unify fieldTy1 fieldTy2
        restRow2
    | TRowExtend (label2, fieldTy2, restRow2) ->
        TRowExtend (label2, fieldTy2, rewriteRow restRow2 label1 fieldTy1)
    | TVar {contents = Link row2 } -> rewriteRow row2 label1 fieldTy1
    | TVar ({contents = Unbound (id, level)} as tvar) ->
        let restRow2 = newVar level
        let ty2 = TRowExtend (label1, fieldTy1, restRow2)
        tvar := Link ty2
        restRow2
    | _ -> raise (ErrorException RowTypeExpected)

let rec generalizeTy level = function
    | TVar {contents = Unbound(id, otherLevel)} when otherLevel > level ->  
        TVar (ref (Generic id))
    | TApp (ty, tyArgList) ->
        TApp (generalizeTy level ty, List.map (generalizeTy level) tyArgList)
    | TArrow (paramTy, returnTy) ->
        TArrow (generalizeTy level paramTy, generalizeTy level returnTy)
    | TVar {contents = Link ty} -> generalizeTy level ty
    | TRecord row -> TRecord (generalizeTy level row)
    // | TVariant row -> TVariant (generalizeTy level row)
    | TRowExtend (label, fieldTy, row) ->
        TRowExtend (label, generalizeTy level fieldTy, generalizeTy level row)
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
        // | TVariant row -> TVariant (f row)
        | TRowEmpty -> ty
        | TRowExtend (label, fieldTy, row) ->
            TRowExtend (label, f fieldTy, f row)
    f ty

let rec matchFunTy = function
    | TArrow (paramTy, returnTy) -> paramTy, returnTy
    | TVar {contents = Link ty} -> matchFunTy ty
    | TVar ({contents = Unbound(id, level)} as tvar) ->
        let paramTy = newVar level
        let returnTy = newVar level
        tvar := Link (TArrow(paramTy, returnTy))
        paramTy, returnTy
    | _ -> raise (ErrorException FunctionExpected)

let rec inferExpr env level = function
    | EValue value -> inferValue env level value
    | EVar name ->
        env
        |> Map.tryFind name
        |> Option.map (instantiate level)
        |> Option.defaultWith (fun () ->
            raise (ErrorException (VariableNotFound name))
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
        let paramTy = TRecord (TRowExtend(label, fieldTy, restRowTy))
        let returnTy = fieldTy
        unify paramTy (inferExpr env level recordExpr)
        returnTy
    | ERecordRestrict (recordExpr, label) ->
        let restRowTy = newVar level
        let fieldTy = newVar level
        let paramTy = TRecord (TRowExtend(label, fieldTy, restRowTy))
        let returnTy = TRecord restRowTy
        unify paramTy (inferExpr env level recordExpr)
        returnTy
    | ERecordExtend (label, expr, recordExpr) ->
        let restRowTy = newVar level
        let fieldTy = newVar level
        let param1Ty = fieldTy
        let param2Ty = TRecord restRowTy
        let returnTy = TRecord (TRowExtend (label, fieldTy, restRowTy))
        unify param1Ty (inferExpr env level expr)
        unify param2Ty (inferExpr env level recordExpr)
        returnTy
    // | EVariant (label, expr) ->
    //     let restRowTy = newVar level
    //     let variantTy = newVar level
    //     let param_ty = variantTy in
    //     let return_ty = TVariant (TRowExtend(Map.singleton label [variantTy], restRowTy))
    //     unify param_ty (inferExpr env level expr)
    //     return_ty
    // | ECase (expr, cases, None) ->
    //     let returnTy = newVar level
    //     let exprTy = inferExpr env level expr
    //     let casesRow = inferCases env level returnTy TRowEmpty cases
    //     unify exprTy (TVariant casesRow)
    //     returnTy
    // | ECase (expr, cases, Some (defaultVarName, defaultExpr)) ->
    //     let defaultVariantTy = newVar level
    //     let returnTy = inferExpr (Map.add defaultVarName (TVariant defaultVariantTy) env) level defaultExpr
    //     let exprTy = inferExpr env level expr
    //     let casesRow = inferCases env level returnTy defaultVariantTy cases
    //     unify exprTy (TVariant casesRow)
    //     returnTy

// and inferCases env level returnTy restRowTy cases =
//     match cases with
//     | [] -> restRowTy
//     | (label, varName, expr) :: otherCases ->
//         let variantTy = newVar level
//         unify returnTy (inferExpr (Map.add varName variantTy env) level expr)
//         let otherCasesRow = inferCases env level returnTy restRowTy otherCases
//         TRowExtend (Map.singleton label [variantTy], otherCasesRow)
   
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
