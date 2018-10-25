module Util

module List =
    let cons x xs = x :: xs

module Map =
    let singleton k v =
        Map.empty
        |> Map.add k v

    let merge m1 m2 =
        (m1, m2)
        ||> Map.fold (fun state key value ->
            Map.add key value state
        )

module Option =
    let bindNone f = function
        | None -> f ()
        | Some _ -> None

let (|>!) x f = f x; x
