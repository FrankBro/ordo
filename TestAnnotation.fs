module TestAnnotation

open Xunit

open Error
open Expr
open TestUtil

[<Fact>]
let ``Let ann`` () =
    test 
        "let (a: int) = 1 in a"
        (POk (ELet (EType (EVar "a", TInt), EInt 1, EVar "a")))
        (IOk "int")
        (EOk (VInt 1))

[<Fact>]
let ``Let wrong ann`` () =
    test 
        "let (a: float) = 1 in a"
        (POk (ELet (EType (EVar "a", TFloat), EInt 1, EVar "a")))
        (IFail (i (UnifyFail (TFloat, TInt))))
        (EOk (VInt 1))
