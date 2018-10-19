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
{y : int, x : int}
```
* For variants:
```ocaml
ordo>>> let variant = :variant 1
:variant 1
ordo>>> :type variant
<variant : int | 'a>
```

The extra syntax at the end of the variant means it is an open variant. Rows (record or variants) can be open or closed. By default, record literals are closed but variant literals are open.