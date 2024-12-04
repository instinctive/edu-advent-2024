# [Day 4](https://adventofcode.com/2024/day/4)

The input is a grid of letters, which we represent as a list of strings.

```haskell top:2
main = do
    rows <- lines <$> getContents
```

# Part 1

We're going to search every horizontal, vertical, and diagonal line for the target word.

```haskell top:2
    print $ sum $ count "XMAS" <$> allLines rows
```

First we need to generate every such line:

```haskell
allLines rows =
    rows <>
    transpose rows <>
    tri rows <>
    tri (reverse rows) <>
    tail (tri (map reverse rows)) <>
    tail (tri (reverse $ map reverse rows))
  where
    tri = transpose . zipWith drop [0..]
```

Now we need to be able to count the words in any line.
We look for it forwards and backwards.

```haskell
count word line =
    check word + check (reverse word)
  where
    check word = length $ filter (isPrefixOf word) $ tails line
```

## Module header and imports

```haskell top
module Main where
```
