module Error

open Expr

type InferError =
    | RecursiveTypes
    | RecursiveRowTypes
    | UnifyFail of Ty * Ty
    | RowMissingLabel of Name
    | RowTypeExpected
    | FunctionExpected
    | VariableNotFound of Name

exception ErrorException of InferError
