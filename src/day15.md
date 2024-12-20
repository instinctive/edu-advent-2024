# [Day 15](https://adventofcode.com/2024/day/15)

```haskell top:3
main = do
    input <- parse <$> getContents
    print $ solve input
```

## Parsing the input

The warehouse map has four kinds of entries.

```haskell top:1
data Item = Empty | Block | Wall | Robot deriving (Eq,Ord,Show)

toItem '.' = Empty
toItem 'O' = Block
toItem '#' = Wall
toItem '@' = Robot
```

We keep an array of all the wall positions, and a set of all the block
positions.

```haskell top:1
type Walls = UArray Pos Bool
type Blocks = Set Pos
```

We parse the input into the robot position, the walls, the blocks, and the
commands for the robot.

```haskell
parse :: String -> (Pos, Walls, Blocks, [Dir])
parse s = (robot, walls, blocks, commands) where
```

The map of the warehouse is separated from the commands by a blank line.

```haskell
    (one@(row:_),_:two) = break null $ lines s
```

The commands are a list of directions for the robot to move.

```haskell
    commands = concat two <&> \case
        '^' -> N
        '>' -> E
        'v' -> S
        '<' -> W
```

We associate each item in the warehouse map with its position.

```haskell
    nrows = length one
    ncols = length row
    items =
        [ (i, V2 r c)
        | (r,xx) <- zip [0..] one
        , (c,i)  <- zip [0..] (toItem <$> xx)
        , i /= Empty ]
```

We get the robot position, the walls array, and the set of blocks.o

```haskell
    (robot, walls, blocks) = runST do
        robot <- newSTRef (V2 0 0)
        walls <- newArray (0, V2 (nrows-1) (ncols-1)) False :: ST s (STUArray s Pos Bool)
        blocks <- newSTRef S.empty

        for_ items \(x,pos) -> case x of
            Robot -> writeSTRef robot pos
            Block -> modifySTRef' blocks (S.insert pos)
            Wall  -> writeArray walls pos True

        (,,) <$> readSTRef robot <*> unsafeFreeze walls <*> readSTRef blocks
```

## Part 1

The answer is a sum of the final GPS coordinates.

```haskell
solve :: (Pos,Walls,Blocks,[Dir]) -> Int
solve (robot,walls,blocks,commands) =
    sum $ map gps $ S.elems final
  where
    gps (V2 r c) = r * 100 + c
    final = execState (foldM go robot commands) blocks
```

To move the robot, we look at the next space. If it's a wall or empty, we're done.

```haskell
    go robot dir = do
        let next = robot + step dir
        lookup next >>= \case
            Empty -> pure next
            Wall  -> pure robot
```

If it's a block, then we look at the first non-block space in that
direction. If it's empty, we move the block.

```haskell
            Block -> check dir next >>= \(item,dest) -> case item of
                Wall -> pure robot
                Empty -> do
                    modify' (S.insert dest . S.delete next)
                    pure next
```

Lookup what item is in a space.

```haskell
    lookup pos
        | walls!pos = pure Wall
        | otherwise = gets (S.member pos) >>= pure . bool Empty Block
```

Check in the given direction for the first non-block space.

```haskell
    check dir pos = lookup pos >>= \case
        Block -> check dir (pos + step dir)
        item -> pure (item,pos)
```

## Module header and imports

```haskell top
module Main where
import Advent
import Data.Array.Unsafe ( unsafeFreeze )
import qualified Data.Set as S
```
