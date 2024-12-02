# Day 1

This problem asks you to read two equal length lists organized by lines in
pairs, sort each list, compare the sorted lists pair by pair, and sum the
absolute values of the difference between each pair.

```haskell top:2
main :: IO ()
main = do
    let parse = map (read @Int) . words
    [aa,bb] <- map sort . transpose . map parse . lines <$> getContents
    let ans = sum $ zipWith (\a b -> abs (a-b)) aa bb
    print ans
```

## Module header

```haskell top
module Main where
```
