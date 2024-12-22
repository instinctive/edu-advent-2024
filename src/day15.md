# [Day 15](https://adventofcode.com/2024/day/15)

```haskell top:3
main = do
    input <- getContents
    print $ solve $ parse $ input
    -- print $ solve $ parse $ concatMap mkPart2 input

parse input =
    (robot, M.fromList items, concatMap toDir $ concat commands)
  where
    (grid,_:commands) = break null $ lines input
    ([(robot,_)],items) = partition ((=='@').snd)
        [ (V2 r c, x)
        | (r,xx) <- zip [0..] grid
        , (c,x)  <- zip [0..] xx
        , x /= '.' ]

toDir '^' = [N]
toDir '>' = [E]
toDir 'v' = [S]
toDir '<' = [W]

solve (robot, grid, commands) =
    score $ foldl' go (grid,robot) commands
  where
    go q@(grid,curr) dir = let next = curr + step dir in
        maybe q (, next) (move (grid,next) dir)

type Grid = Map Pos Char
type St = (Grid, Pos)

score :: St -> Int
score (grid,robot) =
    sum $ gps <$> boxes
  where
    gps (V2 r c) = r * 100 + c
    boxes = M.keys $ M.delete robot $ M.filter (=='O') grid

move :: St -> Dir -> Maybe Grid
move (grid,curr) dir = case lookup curr grid of
    '#' -> Nothing
    '.' -> Just grid
    'O' -> move (grid,next) dir >>= Just . push 'O'
  where
    next = curr + step dir
    lookup = M.findWithDefault '.'
    push x = M.insert next x . M.delete curr
```

## Module header and imports

```haskell top
module Main where
import Advent
import qualified Data.Map.Strict as M
```
