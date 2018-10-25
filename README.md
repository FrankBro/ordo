# Ordo

Latin : ordo

English : a methodical series, arrangement, or order; regular line, row, or series

## Introduction

Ordo is a minimal statically-typed functional programming language with a ml-like syntax and focused around row polymorphism. 

Row polymorphism focuses around allowing you to compose simple types to create more complex types: mainly records and variants.

Where in most ml-languages, you need to pre-define your records and variants, in ordo, they will be infered.

* For records:
```ocaml
ordo>>> let record = { x = 1, y = 2 }
{ x : 1, y : 2 }
ordo>>> :type record
{x : int, y : int}
```
* For variants:
```ocaml
ordo>>> let variant = :variant 1
:variant 1
ordo>>> :type variant
forall r. (r\variant) => <variant : int | r>
```

The extra syntax at the end of the variant means it is an open variant. Rows (record or variants) can be open or closed. By default, record literals are closed but variant literals are open. Rows also infer their restrictions. While the signature can be fairly hard to read at first, here is the meaning of it:

```ocaml
forall r. (r\variant) => <variant : int | r>
```

Here, `forall` signifies that generic types will be given next. We might also have a portion between parenthesis, indicating row restrictions. In this case, for a generic type `r`, which we know is a row because it's followed by the restriction that this row should not have the value `variant`. Next we finally have the actual type of our expression, we know it's a variant because it's surrounded by `< >`, whereas records are surrounded by `{ }`. The type of the expression is a variant that extends the generic row `r` by having a new case that is specified: `variant` of type `int`.
