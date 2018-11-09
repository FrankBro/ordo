module Compiler

open Error
open Expr
open Infer

type State = {
    Types: Map<string, Ty>
    Values: Map<string, Value>
}
with
    static member New = {
        Types = Map.empty
        Values = Map.empty
    }

let compileExprs (exprs: (string * Expr) list) =
    Infer.resetId ()
    let extract state (name: string, expr: Expr) =
        let ordoTy = 
            Infer.infer state.Types expr
            |> generalize
        let ordoVal = Eval.eval state.Values expr
        ordoTy, ordoVal
    let rec loop (state: State) exprs =
        match exprs with
        | [] -> raise (compilerError NoExprsProvided)
        | [x] -> extract state x
        | x :: xs ->
            let ordoTy, ordoVal = extract state x
            let name = fst x
            let state = 
                { state with
                    Types = Map.add name ordoTy state.Types
                    Values = Map.add name ordoVal state.Values
                }
            loop state xs
    loop State.New exprs
