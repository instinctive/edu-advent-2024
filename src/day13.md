# [Day 12](https://adventofcode.com/2024/day/12)

The input is groups of three lines per machine,
each line having two integers.

```haskell top:3
main = do
    input <- map stringToInts . lines . onlyDigits <$> getContents
    let groups = mkGroup <$> splitOn [[]] input
    print $ sum $ part1 <$> groups
  where
    mkGroup (map mkPos -> [a,b,q]) = (a,b,q)
    mkPos [x,y] = V2 x y

mquot a b = case quotRem a b of
    (x,0) -> Just x
    _ -> Nothing

quotPos (V2 x1 y1) (V2 x2 y2) =
    V2 <$> mquot x1 x2 <*> mquot y1 y2

part1 :: (Pos,Pos,Pos) -> Int
part1 (av,bv,qv)
    | null cands = 0
    | otherwise = minimum cands
  where
    cands = mapMaybe go [0..100]
    go :: Int -> Maybe Int
    go a | any (<0) qv' = Nothing
         | otherwise = case quotPos qv' bv of
            Nothing -> Nothing
            Just (V2 x y) | x == y -> Just $ a*3 + x
                          | otherwise -> Nothing
      where
        qv' = qv - V2 a a * av
```

## Module header and imports

```haskell top
module Main where
import Advent
import qualified Data.Map as M
import qualified Data.Set as S
```
