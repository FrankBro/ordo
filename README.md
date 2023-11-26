![example workflow](https://github.com/frankbro/ordo/actions/workflows/rust.yml/badge.svg)

# Ordo

Latin : ordo

English : a methodical series, arrangement, or order; regular line, row, or series

## Introduction

A minimal statically-typed programming language focused around row polymorphism, which allows to infer structural types: records and variants.

* Polymorphic types:
```ocaml
ordo>>> let f(x) = x
forall a => a -> a
fun(x)
```

* Records:
```ocaml
ordo>>> let record = { x = 1, y = 2 }
{x: int, y: int}
{x: 1, y: 2}
```

* Variants:
```ocaml
ordo>>> let variant = :variant 1
forall ra. (ra\variant) => [variant: int | ra]
:variant 1
```

The extra syntax at the end of the variant means it is an open variant. Rows (record or variants) can be open or closed. By default, record literals are closed but variant literals are open. Rows also infer their restrictions. 

Here, `forall` signifies that generic types will be given next. We might also have a portion between parenthesis, indicating row restrictions. In this case, for a generic type `ra`, which we know is a row because it's followed by the restriction that this row should not have the value `variant`. Next we finally have the actual type of our expression, we know it's a variant because it's surrounded by `< >`, whereas records are surrounded by `{ }`. The type of the expression is a variant that extends the generic row `r` by having a new case that is specified: `variant` of type `int`.

## Record

* Record initialization
```ocaml
ordo>>> {}
{}
{}
```
* Record extension adds fields to the record
```ocaml
ordo>>> { x = 1 | {} }
{x: int}
{x: 1}
```
* Record initialization, being sugar for record extension
```ocaml
ordo>>> { x = 1 } == { x = 1 | {} }
bool
true
```
* The order of insertion of labels is not important
```ocaml
ordo>>> { x = 1, y = 2 } == { y = 2, x = 1 }
bool
true
```
* Record restriction removes fields from a record
```ocaml
ordo>>> { x = 1 }\x == {}
bool
true
```
* Record cannot have the same label twice
```ocaml
ordo>>> { x = 1, x = 2 }
error: parser: duplicate label: x
```

In fact, we can see this property being infered by the type system and shown:

```ocaml
ordo>>> let f(r) = { y = 0 | r }
forall ra. (ra\y) => {ra} -> {y: int | ra}
fun(r)
ordo>>> f({ x = 0 })
{x: int, y: int}
{x: 0, y: 0}
```

The signature here specifies that the function takes a row that does not contain the field `y`. If you provide a record with the field `y` already it in, the type inference won't let you.

```ocaml
ordo>>> f({ y = 1 })
error: infer: row constraint failed for label: y
```

* Structual pattern matching and even make sure it is valid via type checking:

```ocaml
ordo>>> let { x = x } = { x = 1 }
{x: int}
{x: 1}
ordo>>> x
int
1
ordo>>> let { y = y } = { x = 1 }
error: infer: missing label: y
```

* Record matching is open

While record literals are closed rows, record matching is open:

```ocaml
ordo>>> let record = { x = 0 }
{x: int}
{x: 0}
ordo>>> let f({ x = x }) = x
forall a ra. (ra\x) => {x: a | ra} -> a
fun({x: x})
```

* Sugar for matching and creation

Records can either directly assign fields to variables in a shorthand syntax or capture variables via closure and set them automatically to their variable name as label:

```ocaml
ordo>>> let f({x,y}) = x + y
forall ra. (ra\x\y) => {x: int, y: int | ra} -> int
fun({x: x, y: y})
ordo>>> let x = 1
int
1
ordo>>> let y = 2
int
2
ordo>>> f({x,y})
int
3
```

## Variant

The few new expressions introduced for variants:

* Variant creation

```ocaml
ordo>>> let variant = :variant 1
forall ra. (ra\variant) => [variant: int | ra]
:variant 1
```

* Variant elimination

Variant can be eliminated via pattern matching. While variant literals are open rows, we can deduce if the input if open on closed based on the fact that a default case is provided or not.

```ocaml
ordo>>> let default_with(default, value) = match value { :some value -> value, :none x -> default }
forall a b => (a, [none: b, some: a]) -> a
fun(default, value)
```

In this case, we did not provide a default case and therefore, the variant was infered to be closed. However, if we wrote it this way:

```ocaml
ordo>>> let is_success(v) = match v { :success a -> true , otherwise -> false }
forall a ra. (ra\success) => [success: a | ra] -> bool
fun(v)
```

This is useful to make sure which variant can be passed in.

```ocaml
ordo>>> is_success(:fail 0)
bool
false
ordo>>> default_with(1, :fail 0)
error: infer: missing label: fail
```
