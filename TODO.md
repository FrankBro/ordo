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

fix_poly:: [[a]->a] -> [a]
fix_poly fl = fix (\self -> map ($ self) fl)
  where fix f = f (fix f)
