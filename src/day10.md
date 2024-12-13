# [Day 10](https://adventofcode.com/2024/day/10)

We parse the input into a terrain array.

```haskell top:3
main :: IO ()
main = do
    terrain <- getArray (map digitToInt) terrainAt
```

## The terrain array

The terrain is an array from positions to heights and trails, where each trail
is represented by the position of its final peak.

```haskell top:1
type Terrain = Array Pos (Int,[Pos])
```

We're going to define `terrainAt`, which returns the height and the trails
at a position, given the terrain array.
The "base case" for this lazy recursive data structure are the peaks.
Each peak has exactly one trail: its own position.

```haskell
terrainAt :: Terrain -> Pos -> Int -> (Int,[Pos])
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
