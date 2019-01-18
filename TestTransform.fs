module TestTransform

open System
open System.IO

open Xunit
open Xunit.Abstractions

open Expr

let testTransform parse transform =
    let parsed = ParserExpr.readExpr parse
    let transformed = Transform.transform parsed
    let parsedTransform = ParserExpr.readExpr transform
    Assert.StrictEqual(parsedTransform, transformed)

[<Fact>]
let ``Let simple record`` () =
    testTransform
        "let { a = a } = { a = 1 } in a"
        "let _var0 = { a = 1 } in let a = _var0.a in a"

[<Fact>]
let ``Let simple variant`` () =
    testTransform
        "let (:a a) = (:a 1) in a"
        "let _var0 = (:a 1) in match _var0 { :a a -> a }"

[<Fact>]
let ``Let record in variant`` () =
    testTransform
        "let (:a { b = b }) = (:a { b = 1 }) in b"
        "let _var0 = (:a { b = 1 }) in match _var0 { :a _var1 -> let b = _var1.b in b }"

[<Fact>]
let ``Let variant in record`` () =
    testTransform
        "let { a = (:b b) } = { a = (:b 1) } in b"
        "let _var0 = { a = (:b 1) } in let _var1 = _var0.a in match _var1 { :b b -> b }"

[<Fact>]
let ``Let imbricked records`` () =
    testTransform
        "let { a = { b = b } } = { a = { b = 1 } } in b"
        "let _var0 = { a = { b = 1 } } in let _var1 = _var0.a in let b = _var1.b in b"

[<Fact>]
let ``Let imbricked variants`` () =
    testTransform
        "let (:a (:b b)) = (:a (:b 1)) in b"
        "let _var0 = (:a (:b 1)) in match _var0 { :a _var1 -> match _var1 { :b b -> b }}"

[<Fact>]
let ``Let record multiple fields`` () =
    testTransform
        "let { a = a, b = b } = { a = 1, b = 2 } in a + b"
        "let _var0 = { a = 1, b = 2 } in let b = _var0.b in let a = _var0.a in a + b"

[<Fact>]
let ``Fun simple record`` () =
    testTransform
        "fun { a = a } -> a"
        "fun _var0 -> let a = _var0.a in a"

[<Fact>]
let ``Fun simple variant`` () =
    testTransform
        "fun (:a a) -> a"
        "fun _var0 -> match _var0 { :a a -> a }"

[<Fact>]
let ``Fun record in variant`` () =
    testTransform
        "fun (:a { b = b }) -> b"
        "fun _var0 -> match _var0 { :a _var1 -> let b = _var1.b in b }"

[<Fact>]
let ``Fun variant in record`` () =
    testTransform
        "fun { a = (:b b) } -> b"
        "fun _var0 -> let _var1 = _var0.a in match _var1 { :b b -> b }"

[<Fact>]
let ``Fun imbricked records`` () =
    testTransform
        "fun { a = { b = b } } -> b"
        "fun _var0 -> let _var1 = _var0.a in let b = _var1.b in b"

[<Fact>]
let ``Fun imbricked variants`` () =
    testTransform
        "fun (:a (:b b)) -> b"
        "fun _var0 -> match _var0 { :a _var1 -> match _var1 { :b b -> b }}"

[<Fact>]
let ``Fun record multiple fields`` () =
    testTransform
        "fun { a = a, b = b } -> a + b"
        "fun _var0 -> let b = _var0.b in let a = _var0.a in a + b"

[<Fact>]
let ``Extract guards in record`` () =
    testTransform
        "match x { { a = 1 } -> 1 }"
        "match x { { a = _var0 } when _var0 = 1 -> 1 }"

[<Fact>]
let ``Extract guards in variant`` () =
    testTransform
        "match x { :a 1 -> 1 }"
        "match x { :a _var0 when _var0 = 1 -> 1 }"

[<Fact>]
let ``Transform does not mess with functions`` () =
    testTransform
        "let add a b = a + b"
        "let add a b = a + b"

[<Fact>]
let ``Extract expr in if`` () =
    testTransform
        "if true then 1 else 0"
        "let _var0 = true in if _var0 then 1 else 0"
