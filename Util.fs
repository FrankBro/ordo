module Util

module List =
    let cons x xs = x :: xs

module Map =
    let singleton k v =
        Map.empty
        |> Map.add k v

let (|>!) x f = f x; x
