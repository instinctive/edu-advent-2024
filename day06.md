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
There are two kinds of terrain: open and blocked.

```haskell top:1
data Terrain = Open | Blocked deriving (Eq,Ord,Show)
type Pos  = V2 Int
type Grid = Array Pos Terrain
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

# Part 1

The answer is the number of unique positions the guard occupies before leaving
the mapped area.

```haskell top:3
    let uniq = S.fromList $ guardPath grid start North
    print $ S.size uniq
```

The guard's path. The guard attempts to move forward.
When the guard is blocked, it turns to the right.

```haskell
guardPath :: Grid -> Pos -> Dir -> [Pos]
guardPath grid = go where
    go curr dir
        | not $ inRange (bounds grid) next = [curr]
        | grid ! next == Blocked = go curr (turnRight dir)
        | otherwise = curr : go next dir
      where next = curr + delta dir
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
