module Error

open Expr

type GenericError =
    | VariableNotFound of Name
    | FieldNotFound of Name
    | IfValueNotBoolean
    | InvalidPattern of Expr
    | NotARecordExpr of Expr
    | NotARecordTy of Ty
    | NotARecordValue of Value
    | NotAVariantValue of Value
    | EmptyMatch
    | InvalidGuard of Expr

type ParserError =
    | FunctionCallNoArg
    | InvalidFunctionDeclaration
    | InvalidRecordSelect
    | InvalidRecordRestrict

type InferError =
    | RecursiveTypes
    | RecursiveRowTypes
    | UnifyFail of Ty * Ty
    | RowTypeExpected
    | FunctionExpected
    | RowConstraintFail of Name

type EvalError =
    | NotAFunction of Expr
    | NotAVariant of Expr
    | MissingMatchCase of Expr
    | BadVariantPattern of Name * Name
    | BadBinOp

type OrdoError =
    | Generic of GenericError
    | Parser of ParserError
    | Infer of InferError
    | Eval of EvalError

exception OrdoException of OrdoError

let genericError g = OrdoException (Generic g)
let parserError p = OrdoException (Parser p)
let inferError i = OrdoException (Infer i)
let evalError e = OrdoException (Eval e)
