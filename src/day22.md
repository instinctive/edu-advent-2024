# [Day 21](https://adventofcode.com/2124/day/21)

```haskell
main = do
    n <- read @Int . head <$> getArgs
    initial <- map (read @Int) . words <$> getContents
    print $ sum $ part1 n <$> initial
    print $ part2 n initial

part1 n = (!!n) . iterate gen

part2 n zz =
    maximum $ M.elems $ M.unionsWith (+) $ monkey n <$> zz

monkey n z =
    foldl' f M.empty $ zip shifts $ drop 4 prices
  where
    f m (shift,price) = M.union m (M.singleton shift price)
    prices = map (`mod` 10) $ iterate gen z
    deltas = take n $ zipWith (-) (tail prices) prices
    shifts = zip4
        (drop 0 deltas)
        (drop 1 deltas)
        (drop 2 deltas)
        (drop 3 deltas)

gen =
    prune .
    (mix <*> (`unsafeShiftL` 11)) .
    prune .
    (mix <*> (`unsafeShiftR` 5)) .
    prune .
    (mix <*> (`unsafeShiftL` 6)) .
    id

mix = xor
prune s = s `mod` 16777216
```

## Module header and imports

```haskell top
module Main where
import Advent
import qualified Data.Set as S
import qualified Data.Map.Strict as M
```
