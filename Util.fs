module Util

module List =
    let cons x xs = x :: xs

let (|>!) x f = f x; x
