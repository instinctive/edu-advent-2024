# [Day 10](https://adventofcode.com/2024/day/10)

We parse the input into a list of lists of heights (integers).

```haskell top:3
main :: IO ()
main = do
    input <- map (map digitToInt) . lines <$> getContents
```

We then turn this input into a terrain array.

```haskell top:3
    let terrain = mkTerrain input
```

## The terrain array

We represent positions as 2D vectors, and the terrain as
an array from positions to heights and trails, where each
trail is represented by the position of its final peak.

```haskell top:1
type Pos = V2 Int
type Terrain = Array Pos (Int,[Pos])
```

We will need to look up adjacent entries to a position.

```haskell
adjacent :: Array Pos a -> Pos -> [a]
adjacent ary pos = mapMaybe (get . (+pos)) deltas where
    get pos | inRange (bounds ary) pos = Just $ ary!pos
            | otherwise = Nothing
deltas = [ V2 1 0, V2 (-1) 0, V2 0 1, V2 0 (-1) ]
```

We're going to define `terrainAt`, which returns the height and the trails
at a position, given the terrain array.

```haskell
terrainAt :: Terrain -> Pos -> Int -> (Int,[Pos])
```

We're going to use this function, which requires the terrain array,
to build the terrain array itself. How is this possible? We can do this because the `Array`
type is lazy.

The "base case" for this lazy recursive data structure are the peaks.
All peaks have exactly one trail: their own positions.

```haskell
terrainAt ary pos 9 = (9,[pos])
```

The trails at any other position are the concatenation of all the
trails from adjacent positions that are exactly one greater in height.

```haskell
terrainAt ary pos h = (h,trails) where
    trails = concat [ tt | (h',tt) <- adjacent ary pos, h+1 == h' ]
```

We can now create the terrain array from the input.

```haskell
mkTerrain :: [[Int]] -> Terrain
mkTerrain rows@(row:_) = ary where
    nrows = length rows
    ncols = length row
    ary = listArray (1, V2 nrows ncols)
        [ terrainAt ary (V2 r c) h
        | (r,row) <- zip [1..] rows
        , (c,h)   <- zip [1..] row ]
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
import Data.Array
import Linear.V2
import qualified Data.Set as S
