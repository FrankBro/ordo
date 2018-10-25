module Test

open System
open System.IO

open Xunit
open Xunit.Abstractions

open Eval
open Error
open Expr
open Util

type ParseResult =
    | POk of Expr
    | PSkip
    | PFail of OrdoError

type InferResult =
    | IOk of string
    | IFail of OrdoError

type EvalResult =
    | EOk of Value
    | ESkip
    | EFail of OrdoError

let test input parserExpected inferExpected evalExpected =
    let parserResult = 
        try
            Parser.readExpr input
            |> POk
        with
        | OrdoException error -> PFail error
    if parserExpected <> PSkip then
        Assert.StrictEqual(parserExpected, parserResult)
    let inferResult =
        match parserResult with
        | PSkip -> failwith "Impossible"
        | PFail e -> IFail e
        | POk expr ->
            try
                Infer.infer expr
                |> stringOfTy
                |> IOk
            with
            | OrdoException error -> IFail error
    Assert.StrictEqual(inferExpected, inferResult)
    let evalResult =
        match parserResult with
        | PSkip -> failwith "Impossible"
        | PFail e -> EFail e
        | POk expr ->
            try
                Eval.eval expr
                |> EOk
            with
            | OrdoException error -> EFail error
    if evalExpected <> ESkip then 
        Assert.StrictEqual(evalExpected, evalResult)

let g e = OrdoError.Generic e
let e e = OrdoError.Eval e
let i e = OrdoError.Infer e

let eRecord xs =
    (ERecordEmpty, xs)
    ||> List.fold (fun record (label, value) ->
        ERecordExtend (label, value, record)
    )

let tRecord xs =
    (TRowEmpty, xs)
    ||> List.fold (fun record (label, value) ->
        TRowExtend (label, value, record)
    )
    |> TRecord

let tVariant xs =
    (TVar {contents = Generic 0}, xs)
    ||> List.fold (fun variant (label, value) ->
        TRowExtend (label, value, variant)
    )
    |> TVariant

let vRecord xs =
    (Map.empty, xs)
    ||> List.fold (fun record (label, value) ->
        Map.add label value record
    )
    |> VRecord

[<Fact>]
let ``Positive integer`` () =
    test 
        "1"
        (POk (EInt 1))
        (IOk "int")
        (EOk (VInt 1))

[<Fact>]
let ``Negative integer`` () =
    test 
        "-1"
        (POk (EInt -1))
        (IOk "int")
        (EOk (VInt -1))

[<Fact>]
let ``Positive float`` () =
    test 
        "3.14"
        (POk (EFloat 3.14))
        (IOk "float")
        (EOk (VFloat 3.14))

[<Fact>]
let ``Negative float`` () =
    test 
        "-3.14"
        (POk (EFloat -3.14))
        (IOk "float")
        (EOk (VFloat -3.14))

[<Fact>]
let ``Float that stops at dot`` () =
    test 
        "3."
        (POk (EFloat 3.))
        (IOk "float")
        (EOk (VFloat 3.))

[<Fact>]
let ``Boolean true`` () =
    test
        "true"
        (POk (EBool true))
        (IOk "bool")
        (EOk (VBool true))

[<Fact>]
let ``Boolean false`` () =
    test
        "false"
        (POk (EBool false))
        (IOk "bool")
        (EOk (VBool false))

[<Fact>]
let ``Unbound variable`` () =
    test
        "a"
        (POk (EVar "a"))
        (IFail (g (VariableNotFound "a")))
        (EFail (g (VariableNotFound "a")))

[<Fact>]
let ``Simple function call`` () =
    test
        "f x"
        (POk (ECall (EVar "f", EVar "x")))
        (IFail (g (VariableNotFound "f")))
        (EFail (g (VariableNotFound "f")))

[<Fact>]
let ``Function call with multiple args`` () =
    test
        "f x y"
        (POk (ECall (ECall (EVar "f", EVar "x"), EVar "y")))
        (IFail (g (VariableNotFound "f")))
        (EFail (g (VariableNotFound "f")))

[<Fact>]
let ``Simple let`` () =
    test
        "let a = 1 in a"
        (POk (ELet (EVar "a", EInt 1, EVar "a")))
        (IOk "int")
        (EOk (VInt 1))

[<Fact>]
let ``Let call function`` () =
    test
        "let f = fun a -> a in f 1"
        (POk (ELet (EVar "f", EFun (EVar "a", EVar "a"), ECall (EVar "f", EInt 1))))
        (IOk "int")
        (EOk (VInt 1))

[<Fact>]
let ``Empty record`` () =
    test
        "{}"
        (POk (eRecord []))
        (IOk "{}")
        (EOk (vRecord []))

[<Fact>]
let ``Record restrict`` () =
    test
        "let r = { a = 1 } in r\\a"
        (POk (ELet (EVar "r", eRecord ["a", EInt 1], ERecordRestrict (EVar "r", "a"))))
        (IOk "{}")
        (EOk (vRecord []))

[<Fact>]
let ``Record extend`` () =
    test
        "let r = { a = 1 } in { b = 2 | r }"
        (POk (ELet (EVar "r", eRecord ["a", EInt 1], ERecordExtend ("b", EInt 2, EVar "r"))))
        (IOk "{a : int, b : int}")
        (EOk (vRecord ["a", VInt 1; "b", VInt 2]))

[<Fact>]
let ``Record select`` () =
    test
        "let r = { a = 1 } in r.a"
        (POk (ELet (EVar "r", eRecord ["a", EInt 1], ERecordSelect (EVar "r", "a"))))
        (IOk "int")
        (EOk (VInt 1))

[<Fact>]
let ``Variant`` () =
    test
        ":a 1"
        (POk (EVariant ("a", EInt 1)))
        (IOk "forall r. (r\\a) => <a : int | r>")
        (EOk (VVariant ("a", VInt 1)))

[<Fact>]
let ``Match variant`` () =
    test
        "match :a 1 { :a a -> 1 , :y a -> 2 }"
        (POk (ECase ((EVariant ("a", EInt 1)), [EVariant ("a", EVar "a"), EInt 1;EVariant ("y", EVar "a"), EInt 2], None)))
        (IOk "int")
        (EOk (VInt 1))

[<Fact>]
let ``Match open variant`` () =
    test
        "match :b 1 { :a a -> 1 | otherwise -> 2 }"
        (POk (ECase ((EVariant ("b", EInt 1)), [EVariant ("a", EVar "a"), EInt 1], Some ("otherwise", EInt 2))))
        (IOk "int")
        (EOk (VInt 2))

[<Fact>]
let ``If then else`` () =
    test
        "if true then 1 else 0"
        (POk (EIfThenElse (EBool true, EInt 1, EInt 0)))
        (IOk "int")
        (EOk (VInt 1))

[<Fact>]
let ``If value must be bool`` () =
    test
        "if 1 then 1 else 0"
        (POk (EIfThenElse (EInt 1, EInt 1, EInt 0)))
        (IFail (i (UnifyFail (TInt, TBool))))
        (EFail (g IfValueNotBoolean))

[<Fact>]
let ``Record pattern`` () =
    test
        "let { a = a } = { a = 1 } in a"
        (POk (ELet (eRecord ["a", EVar "a"], eRecord ["a", EInt 1], EVar "a")))
        (IOk "int")
        (EOk (VInt 1))

[<Fact>]
let ``Record pattern multiple unordered fields`` () =
    test
        "let { a = a, b = b } = { b = 2, a = 1 } in a + b"
        (POk (ELet (eRecord ["a", EVar "a"; "b", EVar "b"], eRecord ["b", EInt 2; "a", EInt 1], EBinOp (EVar "a", Plus, EVar "b"))))
        (IOk "int")
        (EOk (VInt 3))

[<Fact>]
let ``Record patterns dont need all the fields`` () =
    test
        "let { a = a } = { a = 1, b = 2 } in a"
        (POk (ELet (ERecordExtend ("a", EVar "a", ERecordEmpty), eRecord ["a", EInt 1; "b", EInt 2], EVar "a")))
        (IOk "int")
        (EOk (VInt 1))

[<Fact>]
let ``More complex record pattern`` () =
    test
        "let { a = a | r } = { b = 2, a = 1 } in r.b"
        (POk (ELet (ERecordExtend ("a", EVar "a", EVar "r"), eRecord ["b", EInt 2; "a", EInt 1], ERecordSelect (EVar "r", "b"))))
        (IOk "int")
        (EOk (VInt 2))

[<Fact>]
let ``Record pattern in lambda`` () =
    test
        "let f = fun { a = a } -> a in f { a = 1 }"
        (POk (ELet (EVar "f", EFun (ERecordExtend ("a", EVar "a", ERecordEmpty), EVar "a"), ECall (EVar "f", ERecordExtend ("a", EInt 1, ERecordEmpty)))))
        (IOk "int")
        (EOk (VInt 1))

[<Fact>]
let ``Imbricked records`` () =
    test
        "let { a = { b = b } } = { a = { b = 2 } } in b"
        (POk (ELet (ERecordExtend ("a", ERecordExtend ("b", EVar "b", ERecordEmpty), ERecordEmpty), 
                    ERecordExtend ("a", ERecordExtend ("b", EInt 2, ERecordEmpty), ERecordEmpty),
                    EVar "b")))
        (IOk "int")
        (EOk (VInt 2))

[<Fact>]
let ``Variant pattern`` () =
    test
        "let (:a a) = (:a 1) in a"
        (POk (ELet (EVariant ("a", EVar "a"), EVariant ("a", EInt 1), EVar "a")))
        (IOk "int")
        (EOk (VInt 1))

// TODO This probably should infer correctly, even if it makes little sense, should be generic
[<Fact>]
let ``Bad variant pattern`` () =
    test
        "let (:b b) = (:a 1) in b"
        (POk (ELet (EVariant ("b", EVar "b"), EVariant ("a", EInt 1), EVar "b")))
        (IFail (i RowTypeExpected))
        (EFail (e (BadVariantPattern ("b", "a"))))

[<Fact>]
let ``Variant pattern in lambda`` () =
    test
        "let f = fun (:a a) -> a in f (:a 1)"
        (POk (ELet (EVar "f", EFun (EVariant ("a", EVar "a"), EVar "a"), ECall (EVar "f", EVariant ("a", EInt 1)))))
        (IOk "int")
        (EOk (VInt 1))

[<Fact>]
let ``Plus integer`` () =
    test
        "1 + 2"
        (POk (EBinOp (EInt 1, Plus, EInt 2)))
        (IOk "int")
        (EOk (VInt 3))

[<Fact>]
let ``Plus float`` () =
    test
        "1. + 2."
        (POk (EBinOp (EFloat 1., Plus, EFloat 2.)))
        (IOk "float")
        (EOk (VFloat 3.))

[<Fact>]
let ``Binop fail`` () =
    test
        "1 + 2."
        (POk (EBinOp (EInt 1, Plus, EFloat 2.)))
        (IFail (i (UnifyFail (TInt, TFloat))))
        (EFail (e BadBinOp))

[<Fact>]
let ``Minus integer`` () =
    test
        "3 - 2"
        (POk (EBinOp (EInt 3, Minus, EInt 2)))
        (IOk "int")
        (EOk (VInt 1))

[<Fact>]
let ``Minus float`` () =
    test
        "3. - 2."
        (POk (EBinOp (EFloat 3., Minus, EFloat 2.)))
        (IOk "float")
        (EOk (VFloat 1.))

[<Fact>]
let ``Multiply integer`` () =
    test
        "3 * 2"
        (POk (EBinOp (EInt 3, Multiply, EInt 2)))
        (IOk "int")
        (EOk (VInt 6))

[<Fact>]
let ``Multiply float`` () =
    test
        "3. * 2."
        (POk (EBinOp (EFloat 3., Multiply, EFloat 2.)))
        (IOk "float")
        (EOk (VFloat 6.))

[<Fact>]
let ``Divide integer`` () =
    test
        "6 / 2"
        (POk (EBinOp (EInt 6, Divide, EInt 2)))
        (IOk "int")
        (EOk (VInt 3))

[<Fact>]
let ``Divide float`` () =
    test
        "6. / 2."
        (POk (EBinOp (EFloat 6., Divide, EFloat 2.)))
        (IOk "float")
        (EOk (VFloat 3.))
    
[<Fact>]
let ``And true`` () =
    test
        "true && true"
        (POk (EBinOp (EBool true, And, EBool true)))
        (IOk "bool")
        (EOk (VBool true))

[<Fact>]
let ``And false`` () =
    test
        "true && false"
        (POk (EBinOp (EBool true, And, EBool false)))
        (IOk "bool")
        (EOk (VBool false))

[<Fact>]
let ``Or true`` () =
    test
        "true || false"
        (POk (EBinOp (EBool true, Or, EBool false)))
        (IOk "bool")
        (EOk (VBool true))

[<Fact>]
let ``Or false`` () =
    test
        "false || false"
        (POk (EBinOp (EBool false, Or, EBool false)))
        (IOk "bool")
        (EOk (VBool false))

[<Fact>]
let ``Integer Equal true`` () =
    test
        "1 = 1"
        (POk (EBinOp (EInt 1, Equal, EInt 1)))
        (IOk "bool")
        (EOk (VBool true))

[<Fact>]
let ``Integer Equal false`` () =
    test
        "1 = 2"
        (POk (EBinOp (EInt 1, Equal, EInt 2)))
        (IOk "bool")
        (EOk (VBool false))

[<Fact>]
let ``Float Equal true`` () =
    test
        "1. = 1."
        (POk (EBinOp (EFloat 1., Equal, EFloat 1.)))
        (IOk "bool")
        (EOk (VBool true))

[<Fact>]
let ``Float Equal false`` () =
    test
        "1. = 2."
        (POk (EBinOp (EFloat 1., Equal, EFloat 2.)))
        (IOk "bool")
        (EOk (VBool false))

[<Fact>]
let ``Bool Equal true`` () =
    test
        "true = true"
        (POk (EBinOp (EBool true, Equal, EBool true)))
        (IOk "bool")
        (EOk (VBool true))

[<Fact>]
let ``Bool Equal false`` () =
    test
        "true = false"
        (POk (EBinOp (EBool true, Equal, EBool false)))
        (IOk "bool")
        (EOk (VBool false))

[<Fact>]
let ``Integer NotEqual true`` () =
    test
        "1 <> 2"
        (POk (EBinOp (EInt 1, NotEqual, EInt 2)))
        (IOk "bool")
        (EOk (VBool true))

[<Fact>]
let ``Integer NotEqual false`` () =
    test
        "1 <> 1"
        (POk (EBinOp (EInt 1, NotEqual, EInt 1)))
        (IOk "bool")
        (EOk (VBool false))

[<Fact>]
let ``Float NotEqual true`` () =
    test
        "1. <> 2."
        (POk (EBinOp (EFloat 1., NotEqual, EFloat 2.)))
        (IOk "bool")
        (EOk (VBool true))

[<Fact>]
let ``Float NotEqual false`` () =
    test
        "1. <> 1."
        (POk (EBinOp (EFloat 1., NotEqual, EFloat 1.)))
        (IOk "bool")
        (EOk (VBool false))

[<Fact>]
let ``Bool NotEqual true`` () =
    test
        "true <> false"
        (POk (EBinOp (EBool true, NotEqual, EBool false)))
        (IOk "bool")
        (EOk (VBool true))

[<Fact>]
let ``Bool NotEqual false`` () =
    test
        "true <> true"
        (POk (EBinOp (EBool true, NotEqual, EBool true)))
        (IOk "bool")
        (EOk (VBool false))

[<Fact>]
let ``Integer Greater true`` () =
    test
        "2 > 1"
        (POk (EBinOp (EInt 2, Greater, EInt 1)))
        (IOk "bool")
        (EOk (VBool true))

[<Fact>]
let ``Integer Greater false`` () =
    test
        "1 > 2"
        (POk (EBinOp (EInt 1, Greater, EInt 2)))
        (IOk "bool")
        (EOk (VBool false))

[<Fact>]
let ``Float Greater true`` () =
    test
        "2. > 1."
        (POk (EBinOp (EFloat 2., Greater, EFloat 1.)))
        (IOk "bool")
        (EOk (VBool true))

[<Fact>]
let ``Float Greater false`` () =
    test
        "1. > 2."
        (POk (EBinOp (EFloat 1., Greater, EFloat 2.)))
        (IOk "bool")
        (EOk (VBool false))

[<Fact>]
let ``Integer GreaterEqual true`` () =
    test
        "2 >= 2"
        (POk (EBinOp (EInt 2, GreaterEqual, EInt 2)))
        (IOk "bool")
        (EOk (VBool true))

[<Fact>]
let ``Integer GreaterEqual false`` () =
    test
        "1 >= 2"
        (POk (EBinOp (EInt 1, GreaterEqual, EInt 2)))
        (IOk "bool")
        (EOk (VBool false))

[<Fact>]
let ``Float GreaterEqual true`` () =
    test
        "2. >= 2."
        (POk (EBinOp (EFloat 2., GreaterEqual, EFloat 2.)))
        (IOk "bool")
        (EOk (VBool true))

[<Fact>]
let ``Float GreaterEqual false`` () =
    test
        "1. >= 2."
        (POk (EBinOp (EFloat 1., GreaterEqual, EFloat 2.)))
        (IOk "bool")
        (EOk (VBool false))

[<Fact>]
let ``Integer Lesser true`` () =
    test
        "2 < 3"
        (POk (EBinOp (EInt 2, Lesser, EInt 3)))
        (IOk "bool")
        (EOk (VBool true))

[<Fact>]
let ``Integer Lesser false`` () =
    test
        "3 < 2"
        (POk (EBinOp (EInt 3, Lesser, EInt 2)))
        (IOk "bool")
        (EOk (VBool false))

[<Fact>]
let ``Float Lesser true`` () =
    test
        "2. < 3."
        (POk (EBinOp (EFloat 2., Lesser, EFloat 3.)))
        (IOk "bool")
        (EOk (VBool true))

[<Fact>]
let ``Float Lesser false`` () =
    test
        "3. < 2."
        (POk (EBinOp (EFloat 3., Lesser, EFloat 2.)))
        (IOk "bool")
        (EOk (VBool false))

[<Fact>]
let ``Integer LesserEqual true`` () =
    test
        "2 <= 2"
        (POk (EBinOp (EInt 2, LesserEqual, EInt 2)))
        (IOk "bool")
        (EOk (VBool true))

[<Fact>]
let ``Integer LesserEqual false`` () =
    test
        "3 <= 2"
        (POk (EBinOp (EInt 3, LesserEqual, EInt 2)))
        (IOk "bool")
        (EOk (VBool false))

[<Fact>]
let ``Float LesserEqual true`` () =
    test
        "2. <= 2."
        (POk (EBinOp (EFloat 2., LesserEqual, EFloat 2.)))
        (IOk "bool")
        (EOk (VBool true))

[<Fact>]
let ``Float LesserEqual false`` () =
    test
        "3. <= 2."
        (POk (EBinOp (EFloat 3., LesserEqual, EFloat 2.)))
        (IOk "bool")
        (EOk (VBool false))

[<Fact>]
let ``Record pattern in match`` () =
    test
        "match :a { a = 1 } { :a { a = a } -> a }"
        (POk (ECase (EVariant ("a", eRecord ["a", EInt 1]), [EVariant ("a", eRecord ["a", EVar "a"]), EVar "a"], None)))
        (IOk "int")
        (EOk (VInt 1))

[<Fact>]
let ``Variant pattern in match`` () =
    test
        "match :a (:b 2) { :a (:b b) -> b }"
        (POk (ECase (EVariant ("a", EVariant ("b", EInt 2)), [EVariant ("a", EVariant ("b", EVar "b")), EVar "b"], None)))
        (IOk "int")
        (EOk (VInt 2))

[<Fact>]
let ``Function sugar in let`` () =
    test
        "let f a b = a + b in f 1 2"
        (POk (ELet (EVar "f", EFun (EVar "a", EFun (EVar "b", EBinOp (EVar "a", Plus, EVar "b"))), ECall (ECall (EVar "f", EInt 1), EInt 2))))
        (IOk "int")
        (EOk (VInt 3))

[<Fact>]
let ``Function multiple args`` () =
    test
        "let f = fun a b -> a + b in f 1 2"
        (POk (ELet (EVar "f", EFun (EVar "a", EFun (EVar "b", EBinOp (EVar "a", Plus, EVar "b"))), ECall (ECall (EVar "f", EInt 1), EInt 2))))
        (IOk "int")
        (EOk (VInt 3))

[<Fact>]
let ``Binop a record field`` () =
    test
        "let r = { x = 0 } in r.x + 1"
        (POk (ELet (EVar "r", eRecord ["x", EInt 0], EBinOp (ERecordSelect (EVar "r", "x"), Plus, EInt 1))))
        (IOk "int")
        (EOk (VInt 1))

[<Fact>]
let ``Record match sugar`` () =
    test
        "let f {x,y} = x + y in f { x = 1, y = 2 }"
        (POk (ELet (EVar "f", EFun (eRecord ["x", EVar "x"; "y", EVar "y"], EBinOp (EVar "x", Plus, EVar "y")), ECall (EVar "f", eRecord ["x", EInt 1; "y", EInt 2]))))
        (IOk "int")
        (EOk (VInt 3))

[<Fact>]
let ``Record match to construct`` () =
    test
        "let x = 1 in let y = 2 in {x,y}"
        (POk (ELet (EVar "x", EInt 1, ELet (EVar "y", EInt 2, eRecord ["x", EVar "x"; "y", EVar "y"]))))
        (IOk "{x : int, y : int}")
        (EOk (VRecord (["x", VInt 1; "y", VInt 2] |> Map.ofList)))

[<Fact>]
let ``Record restriction is infered on extension`` () =
    test
        "fun r -> { x = 0 | r }"
        (POk (EFun (EVar "r", ERecordExtend ("x", EInt 0, EVar "r"))))
        (IOk "forall r. (r\\x) => {r} -> {x : int | r}")
        ESkip

[<Fact>]
let ``Record restriction is infered on selection`` () =
    test
        "fun r -> r.x"
        (POk (EFun (EVar "r", ERecordSelect (EVar "r", "x"))))
        (IOk "forall a r. (r\\x) => {x : a | r} -> a")
        ESkip

[<Fact>]
let ``Record restriction is infered on restriction`` () =
    test
        "fun r -> r\\x"
        (POk (EFun (EVar "r", ERecordRestrict (EVar "r", "x"))))
        (IOk "forall a r => {x : a | r} -> {r}")
        ESkip

[<Fact>]
let ``Variant restriction on literal`` () =
    test
        ":a 0"
        (POk (EVariant ("a", EInt 0)))
        (IOk "forall r. (r\\a) => <a : int | r>")
        (EOk (VVariant ("a", VInt 0)))

[<Fact>]
let ``Variant restriction on closed match`` () =
    test
        "fun r -> match r { :x x -> 0 }"
        (POk (EFun (EVar "r", ECase (EVar "r", [EVariant ("x", EVar "x"), EInt 0], None))))
        (IOk "forall a => <x : a> -> int")
        ESkip

[<Fact>]
let ``Variant restriction on open match`` () =
    test
        "fun r -> match r { :x x -> 0 | otherwise -> 1 }"
        (POk (EFun (EVar "r", ECase (EVar "r", [EVariant ("x", EVar "x"), EInt 0], Some ("otherwise", EInt 1)))))
        (IOk "forall a r. (r\\x) => <x : a | r> -> int")
        ESkip

[<Fact>]
let ``If variant restriction`` () =
    test
        "if true then :a 0 else :b 1"
        (POk (EIfThenElse (EBool true, EVariant ("a", EInt 0), EVariant ("b", EInt 1))))
        (IOk "forall r. (r\\a\\b) => <a : int, b : int | r>")
        (EOk (VVariant ("a", VInt 0)))

[<Fact>]
let ``Record restriction for multiple fields`` () =
    test
        "fun r -> { x = 0, y = 0 | r }"
        (POk (EFun (EVar "r", ERecordExtend ("y", EInt 0, ERecordExtend ("x", EInt 0, EVar "r")))))
        (IOk "forall r. (r\\x\\y) => {r} -> {x : int, y : int | r}")
        ESkip

[<Fact>]
let ``Record arg sugar bug`` () =
    test
        "let f {x,y} = x + y in 
         let x = 1 in 
         let y = 2 in 
         f {x,y}"
        PSkip
        (IOk "int")
        (EOk (VInt 3))

[<Fact>]
let ``Same field in record twice is invalid`` () =
    test
        "{x = 0, x = 1}"
        (POk (ERecordExtend ("x", EInt 1, ERecordExtend ("x", EInt 0, ERecordEmpty))))
        (IFail (i (RowConstraintFail "x")))
        ESkip

[<Fact>]
let ``If expressions can just use a boolean variable`` () =
    test
        "let f bool = if bool then 1 else 0 in f true"
        (POk (ELet (EVar "f", EFun (EVar "bool", EIfThenElse (EVar "bool", EInt 1, EInt 0)), ECall (EVar "f", EBool true))))
        (IOk "int")
        (EOk (VInt 1))

[<Fact>]
let ``Record equality works`` () =
    test
        "{ x = 0 } = { x = 0 }"
        (POk (EBinOp (ERecordExtend ("x", EInt 0, ERecordEmpty), Equal, ERecordExtend ("x", EInt 0, ERecordEmpty))))
        (IOk "bool")
        (EOk (VBool true))

// Shitty that I need parenthesis here
[<Fact>]
let ``Variant equality works`` () =
    test
        "(:a 0) = (:a 0)"
        (POk (EBinOp (EVariant ("a", EInt 0), Equal, EVariant ("a", EInt 0))))
        (IOk "bool")
        (EOk (VBool true))
