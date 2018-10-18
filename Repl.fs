module Repl

open Expr
open Infer
open Eval

type Env = {
    Infer: Map<Name, Ty>
    Eval: Map<Name, Value>
}

let flushStr str = printf "%s" str

let readPrompt prompt = 
    flushStr prompt
    System.Console.ReadLine ()

let evalString (env: Env) expr =
    try
        let ordoExpr = Parser.readExpr expr
        let ordoTyp = Infer.inferExpr env.Infer 0 ordoExpr
        let ordoVal = Eval.evalExpr env.Eval ordoExpr
        let env =
            match ordoExpr with
            | ELet (name, _, _) ->
        Result.Ok ()
        let result = Eval.eval env lispVal
        string result
    with 
    | e -> 
        Result.Error (sprintf "Error: %A" e)

let evalAndPrint env expr =
    let env, value, typ, output = evalString env expr
    printfn "%s" (evalString env expr)
    env

let rec until env pred prompt action =
    let result = prompt ()
    if pred result then
        ()
    else
        let env = action env result
        until env pred prompt action

let runRepl () =
    resetId ()
    let env = Map.empty
    until env ((=) "quit") (fun () -> readPrompt "ordo>>> ") evalAndPrint