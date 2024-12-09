# [Day 6](https://adventofcode.com/2024/day/6)

The input has a grid of terrain, and the starting position of the guard.
We extract both while parsing.

```haskell top:3
main = do
    rows <- lines <$> getContents
    let (grid,start) = parse rows
```

## Grid representation

The grid is represented as an array from 2D vector positions to terrain.
The terrain at any position can be out of bounds, open, or blocked.

```haskell top:1
data Terrain = OOB | Open | Blocked deriving (Eq,Ord,Show)
type Pos  = V2 Int
type Grid = Array Pos Terrain
```

The `terrain` function reports the terrain at a given position,
and handles the case where the position is off the map.

```haskell top:3
    let terrain pos | not (inRange (bounds grid) pos) = OOB
        terrain pos = grid!pos
```

We will be working with cardinal directions in this problem.

```haskell top:1
data Dir = North | East | South | West deriving (Eq,Ord,Enum,Show)
```

Right-handed turns.

```haskell
turnRight West = North
turnRight d = succ d
```

Each direction is associated with a positional delta in the grid.

```haskell
delta = \case
    North -> V2 n 0
    South -> V2 s 0
    West  -> V2 0 w
    East  -> V2 0 e
  where
    n = -1; s = 1; w = -1; e = 1
```

## Parsing

We parse the input lines into a grid while also grabbing the position of the guard.

```haskell
parse :: [String] -> (Grid,Pos)
parse rows@(row:_) =
    flip runState 0 do
        listArray (1, V2 nrows ncols) <$> traverse parseTerrain elts
  where
    elts = [ (V2 r c, x) | (r,row) <- zip [1..] rows, (c,x) <- zip [1..] row ]
    ncols = length row
    nrows = length rows
```

Given a `(Pos,Char)` pair,
set the guard's position in the state monad if we find it,
then return the appropriate terrain.

```haskell
parseTerrain :: (Pos,Char) -> State Pos Terrain
parseTerrain (pos,x) = case x of
    '^' -> put pos >> pure Open
    '.' ->            pure Open
    '#' ->            pure Blocked
    x -> error $ "invalid map character: " <> show x
```

## Part 1

The answer is the number of unique positions the guard occupies before leaving
the mapped area.

```haskell top:3
    let uniq = S.fromList $ fst <$> guardPath terrain (start,North)
    print $ S.size uniq
```

The guard's status is a position and direction pair.

```haskell top:1
type Guard = (Pos,Dir)
```

The guard's path. The guard attempts to move forward.
When the guard is blocked, it turns to the right.

```haskell
guardPath :: (Pos -> Terrain) -> Guard -> [Guard]
guardPath terrain = go where
    go guard@(curr,dir) = case terrain next of
        OOB     -> [guard]
        Open    -> guard : go (next,dir)
        Blocked -> go (curr, turnRight dir)
      where next = curr + delta dir
```

## Part 2

We will be looking for paths that have a loop.

```haskell
hasLoop :: [Guard] -> Bool
hasLoop = go S.empty where
    go _ [] = False
    go s (g:gg) = S.member g s || go (S.insert g s) gg
```

The only candidate positions for a new obstacle are the positions the guard
is already visiting. The start position is exempted.

```haskell top:3
    let cands = S.delete start uniq
```

We modify the `terrain` function to add an obstacle at a candidate position.

```haskell top:3
    let mkTerrain obstacle pos
            | pos == obstacle = Blocked
            | otherwise = terrain pos
```

The candidate paths are calculated with respect to a candidate obstacle.

```haskell top:3
    let candPaths = flip guardPath (start,North) . mkTerrain <$> S.toList cands
```

Finally, the answer is the number of those paths that have a loop.

```haskell top:3
    print $ length $ filter hasLoop candPaths
```

## Module header and imports

```haskell top
module Main where
import Control.Monad.State ( State, runState, put )
import Data.Array
import Linear.V2
import qualified Data.Array as A
import qualified Data.Set as S
```
