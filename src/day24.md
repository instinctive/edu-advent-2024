# [Day 24](https://adventofcode.com/2124/day/24)

```haskell
main = do
    (values,rules) <- parse <$> getContents
    print $ part1 values rules

data Op = OpAnd | OpOr | OpXor deriving (Eq,Ord,Show)

parse s =
    (mkvalue <$> svalues, mkrule <$> srules)
  where
    (svalues,_:srules) = break null $ lines $ map tr s
    tr ':' = ' '; tr x = x
    mkvalue (words -> [k,v]) = (T.pack k, read @Int v)
    mkrule (map T.pack . words -> [a,g,b,_,c]) = (c,(op g,sort [a,b]))
    op "AND" = OpAnd
    op "OR"  = OpOr
    op "XOR" = OpXor

part1 vv rr =
    foldl' (\z x -> z*2 + x) 0  $
    map snd                     $
    reverse                     $
    filter ((=='z').T.head.fst) $
    M.assocs m
  where
    m = M.fromList $ vv <> map mk rr
    mk (c,(op,[a,b])) = (c, gate op (m M.! a) (m M.! b))
    gate OpAnd = (.&.)
    gate OpOr  = (.|.)
    gate OpXor = xor

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
