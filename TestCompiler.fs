module TestCompiler

open Xunit

open Compiler
open Error
open Expr
open Parse
open TestUtil

// let testCompiler files (inferExpected: InferResult) (evalExpected: EvalResult) =
//     let exprs =
//         files
//         |> List.map (fun (name, input) ->
//             name, parse input
//         )
//     let ordoTy, ordoVal = compile exprs
//     if inferExpected <> ISkip then
//         Assert.StrictEqual(inferExpected, IOk (stringOfTy ordoTy))
//     if evalExpected <> ESkip then 
//         Assert.StrictEqual(evalExpected, EOk ordoVal)


// [<Fact>]
// let ``Simple two files`` () =
//     testCompiler
//         [
//             "lib.ordo", "1"
//             "main.ordo", "let a = open \"lib.ordo\" in a"
//         ]
//         (IOk "int")
//         (EOk (VInt 1))

// [<Fact>]
// let ``More complex`` () =
//     testCompiler
//         [
//             "first.ordo", "{ a = 1 }"
//             "second.ordo", "{ b = 2 }"
//             "third.ordo", "let first = open \"first.ordo\" in let second = open \"second.ordo\" in first.a + second.b"
//         ]
//         (IOk "int")
//         (EOk (VInt 3))
