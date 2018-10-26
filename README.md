# Ordo

Latin : ordo

English : a methodical series, arrangement, or order; regular line, row, or series

## Introduction

Ordo is a minimal statically-typed functional programming language with a ml-like syntax and focused around row polymorphism. 

Row polymorphism focuses around allowing you to compose simple types to create more complex types: mainly records and variants.

Where in most ml-languages, you need to pre-define your records and variants, in ordo, they will be infered.

### For records:
```ocaml
ordo>>> let record = { x = 1, y = 2 }
{ x : 1, y : 2 }
ordo>>> :type record
{x : int, y : int}
```
### For variants:
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

## Record

The few new expressions introduced for records:

* Record initialization
```ocaml
ordo>>> {}
{  }
```
* Record extension adds fields to the record
```ocaml
ordo>>> { x = 1 | {} }
{ x : 1 }
```
* Record initialization, being sugar for record extension
```ocaml
ordo>>> { x = 1 } = { x = 1 | {} }
True
```
* The order of insertion of labels is not important
```ocaml
ordo>>> { x = 1, y = 2 } = { y = 2, x = 1 }
True
```
* Record restriction removes fields from a record
```ocaml
ordo>>> { x = 1 }\x = {}
True
```
* Record cannot have the same label twice
```ocaml
ordo>>> { x = 1, x = 2 }
Error: OrdoException (Infer (RowConstraintFail "x"))
```

In fact, we can see this property being infered by the type system and shown:

```ocaml
ordo>>> let f r = { y = 0 | r }
<Lambda>
ordo>>> f { x = 0 }
{ x : 0, y : 0 }
ordo>>> :type f
forall r. (r\y) => {r} -> {y : int | r}
```

The signature here specifies that the function takes a row that does not contain the field `y`. If you provide a record with the field `y` already it in, the type inference won't let you.

```ocaml
ordo>>> f { y = 1 }
Error: OrdoException (Infer (RowConstraintFail "y"))
```

* Structual pattern matching and even make sure it is valid via type checking:

```ocaml
ordo>>> let { x = x } = { x = 1 }
{ x : 1 }
ordo>>> x
1
ordo>>> let { y = y } = { x = 1 }
Error: OrdoException (Generic (FieldNotFound "y"))
```

* Record matching is open

While record literals are closed rows, record matching is open:

```ocaml
ordo>>> let record = { x = 0 }
{ x : 0 }
ordo>>> :type record
{x : int}
ordo>>> let f { x = x } = x
<Lambda>
ordo>>> :type f
forall a r. (r\x) => {x : a | r} -> a
```

* Sugar for matching and creation

Records can either directly assign fields to variables in a shorthand syntax or capture variables via closure and set them automatically to their variable name as label:

```ocaml
ordo>>> let f {x,y} = x + y
<Lambda>
ordo>>> let x = 1
1
ordo>>> let y = 2
2
ordo>>> f {x,y}
3
ordo>>> :type f
forall a r. (r\x\y) => {x : a, y : a | r} -> a
```

## Variant

The few new expressions introduced for variants:

* Variant creation

```ocaml
ordo>>> let variant = :variant 1
:variant 1
```

* Variant elimination

Variant can be eliminated via pattern matching. While variant literals are open rows, we can deduce if the input if open on closed based on the fact that a default case is provided or not.

TODO