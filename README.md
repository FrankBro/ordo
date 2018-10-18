# Ordo

Latin : ordo

English : a methodical series, arrangement, or order; regular line, row, or series

## Introduction

Ordo is a minimal statically-typed functional programming language with a ml-like syntax and focused around row polymorphism. 

Row polymorphism focuses around allowing you to compose simple types to create more complex types: mainly records and variants.

Where in most ml-languages, you need to pre-define your records and variants, in ordo, they will be infered.

* For records:
```ocaml
let record = { x = 1, y = 2 }
record : { x: int, y: int }
```
* For variants:
```ocaml
let variant = Variant 1
variant: <Variant: int | 'r>
```