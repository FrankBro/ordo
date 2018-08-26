module Error

open Expr

type GenericError =
    | VariableNotFound of Name
    | FieldNotFound of Name
    | IfValueNotBoolean

type ParserError =
    | FunctionCallNoArg

type InferError =
    | RecursiveTypes
    | RecursiveRowTypes
    | UnifyFail of Ty * Ty
    | RowTypeExpected
    | FunctionExpected

type EvalError =
    | NotAFunction of Expr
    | NotARecord of Expr
    | NotAVariant of Expr
    | MissingMatchCase of Expr

type OrdoError =
    | Generic of GenericError
    | Parser of ParserError
    | Infer of InferError
    | Eval of EvalError

exception ErrorException of OrdoError

let genericError g = ErrorException (Generic g)
let parserError p = ErrorException (Parser p)
let inferError i = ErrorException (Infer i)
let evalError e = ErrorException (Eval e)

