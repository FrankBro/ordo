# TODO

* Add guard syntax sugar. 

```ocaml
match x { :a a when a = 0 -> 0 } 
```

could be

```ocaml
match x { :a 0 -> 0 }
```

* Support pattern matching on record with guards.
