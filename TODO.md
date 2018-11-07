# TODO

fix_poly:: [[a]->a] -> [a]
fix_poly fl = fix (\self -> map ($ self) fl)
  where fix f = f (fix f)
