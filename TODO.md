# TODO

* Mutually recursive functions

```haskell
fix_poly:: [[a]->a] -> [a]
fix_poly fl = fix (\self -> map ($ self) fl)
  where fix f = f (fix f)
```

* Location

```fsharp
type Position = {
    Line: int
    Column: int
}

type Location = {
    Start: Position
    End: Position
    Filename: string
}
```

* Split like labrys

    3 steps?

        Parsed
        Typed
        Emited

* To start llvm should use Boehm GC
