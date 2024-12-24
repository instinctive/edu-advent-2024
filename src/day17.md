# [Day 17](https://adventofcode.com/2024/day/17)

```haskell
main = do
    putStrLn . solve . parse =<< getContents

parse = map (read @Int) . words . only isDigit ' '

solve (a:b:c:xx) =
    intercalate "," . reverse $
    flip execState [] $
    go a b c 0
  where
    maxp = length xx - 1
    code :: UArray Int Int
    code = listArray (0, maxp) xx
    go a b c p | p > maxp = pure ()
    go a b c p = let x = code!(p+1) in case code!p of
        0 -> go (unsafeShiftR a $ combo x) b c (p+2)
        1 -> go a (b `xor` x) c (p+2)
        2 -> go a (mod8 $ combo x) c (p+2)
        3 -> if a == 0 then go a b c (p+2) else go a b c x
        4 -> go a (b `xor` c) c (p+2)
        5 -> out (combo x) >> go a b c (p+2)
        6 -> go a (unsafeShiftR a $ combo x) c (p+2)
        7 -> go a b (unsafeShiftR a $ combo x) (p+2)
      where
        combo 4 = a
        combo 5 = b
        combo 6 = c
        combo x = x

mod8 = (.&.) 0b111
out x = modify' (show (mod8 x):)
```

## Module header and imports

```haskell top
module Main where
import Advent
```
