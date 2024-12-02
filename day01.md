# Day 1

This problem asks you to read two equal length lists organized by lines in
pairs, sort each list, compare the sorted lists pair by pair, and sum the
absolute values of the difference between each pair.

The first thing we'll do is read the integers in each line. This gives us a
list of lists, which we transpose to get the two lists we have to work with.

```haskell
main :: IO ()
main = do
    let parse = map (read @Int) . words
    [aa,bb] <- map sort . transpose . map parse . lines <$> getContents
```

For Part 1, we sum the absolute differences between pairs of these two sorted
lists:

```haskell
    let part1 = sum $ zipWith (\a b -> abs (a-b)) aa bb
    print part1
```

## Module header

```haskell top
module Main where
```
