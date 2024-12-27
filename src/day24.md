# [Day 24](https://adventofcode.com/2124/day/24)

```haskell
main = do
    m <- parse <$> getContents
    let ans1 = part1 m
    print ans1

part1 :: Map Text Bool -> Int
part1 m =
    foldl' (\z x -> z*2 + x) 0  $
    map (bool 0 1 . snd)        $
    reverse                     $
    filter ((=='z').T.head.fst) $
    M.assocs m

parse s =
    m
  where
    m = M.fromList $ (mkval <$> svals) <> (mkrule <$> srules)
    tr ':' = ' '; tr x = x
    (svals,_:srules) = break null $ lines $ map tr s
    mkval (words -> [k,v]) = (T.pack k, v == "1")
    mkrule (map T.pack . words -> [a,g,b,_,c]) = (c,value) where
        value = gate g (m M.! a) (m M.! b)
    gate "AND" = (&&)
    gate "OR"  = (||)
    gate "XOR" = xor

```

## Module header and imports

```haskell top
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Advent
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text ( Text )
```
