# [Day 14](https://adventofcode.com/2024/day/14)

```haskell top:3
main = do
    [sz] : robots <- map parse . lines . map tr <$> getContents
    -- print sz
    -- traverse_ print robots
    let m = mkMap sz robots
    print $ product . map snd $ filter (all (/=0).fst) $ M.assocs m
  where
    mkMap sz robots = M.fromListWith (+) $ go sz <$> robots
    go sz robot = (quad sz $ move sz 100 robot, 1)
    quad sz pos = signum <$> pos - half
      where half = (`div` 2) <$> sz
    move sz n [pos,vel] = 
        mod <$> raw <*> sz
      where
        raw = pos + n * sz + n * vel
    tr c | isDigit c = c
         | isSpace c = c
         | c == '-'  = c
         | otherwise = ' '
    parse = map mk . chunksOf 2 . stringToInts
    mk [x,y] = V2 x y
```

## Module header and imports

```haskell top
module Main where
import Advent
import qualified Data.Map.Strict as M
```
