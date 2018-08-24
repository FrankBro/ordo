module Util

module List =
    let cons x xs = x :: xs

module Map =
    let singleton k v =
        Map.empty
        |> Map.add k v

module Option =
    let bindNone f = function
        | None -> f ()
        | Some _ -> None

let (|>!) x f = f x; x
