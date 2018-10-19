module Repl

open Expr
open Infer
open Eval

type Env = {
    Infer: Map<Name, Ty>
    Eval: Map<Name, Value>
}
with
    static member Empty = {
        Infer = Map.empty
        Eval = Map.empty
    }

let flushStr str = printf "%s" str

let readPrompt prompt = 
    flushStr prompt
    System.Console.ReadLine ()

let evalString (env: Env) expr : Env * string =
    try
        let ordoExpr = Parser.readExpr expr
        match ordoExpr with
        | EVariant ("type", EVar name) ->
            env, Map.find name env.Infer |> stringOfTy
        | EVariant ("value", EVar name) ->
            env, Map.find name env.Eval |> stringOfValue
        | _ ->
            let ordoTyp = Infer.inferExpr env.Infer 0 ordoExpr
            let ordoVal = Eval.evalExpr env.Eval ordoExpr
            let env =
                match ordoExpr with
                | ELet (pat, _, ret) when pat = ret ->
                    let _, infered = inferPattern env.Infer 0 pat
                    let evaled = evalPattern env.Eval pat ordoVal
                    {
                        Infer = infered
                        Eval = evaled
                    }
                | _ ->
                    env
            env, stringOfValue ordoVal
    with 
    | e -> 
        env, (sprintf "Error: %A" e)

let evalAndPrint env expr : Env =
    let env, output = evalString env expr
    printfn "%s" output
    env

let rec until env pred prompt (action: Env -> string -> Env) =
    let result = prompt ()
    if pred result then
        ()
    else
        let env = action env result
        until env pred prompt action

let runRepl () =
    resetId ()
    let env = Env.Empty
    printfn "':type name' for type, ':value name' for value"
    until env ((=) "quit") (fun () -> readPrompt "ordo>>> ") evalAndPrint
