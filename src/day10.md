# [Day 10](https://adventofcode.com/2024/day/10)

We parse the input into a terrain array from positions to heights and trails,
where each trail is represented by the position of its final peak.

```haskell top:1
type Terrain = Array Pos (Int,[Pos])
```

```haskell top:3
main :: IO ()
main = do
    terrain <- getArray (map digitToInt) terrainAt :: IO Terrain
```

## The terrain array

Each peak has exactly one trail: its own position.

```haskell
terrainAt ary pos 9 = (9,[pos])
```

The trails at any other position are the concatenation of all the
trails from adjacent positions that are exactly one greater in height.

```haskell
terrainAt ary pos h = (h,trails) where
    trails = concat
        [ tt 
        | (h',tt) <- (ary!) <$> neighbors ary pos
        , h+1 == h' ]
```

## Part 1

For part 1, we want to know how many different peaks are reachable
from each trailhead.

```haskell top:3
    print $ sum [ S.size $ S.fromList tt | (0,tt) <- elems terrain ]
```

## Part 2

For part 2, we want to know how many different trails (possibly to the same
peak) are at each trailhead.

```haskell top:3
    print $ sum [ length tt | (0,tt) <- elems terrain ]
```

## Module header and imports

```haskell top
module Main where
import Advent
import qualified Data.Set as S
```
