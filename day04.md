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
allLines rows = ortho rows <> diags rows
```

The orthogonal lines are just the rows as given and the transpose of those rows.

```haskell
ortho rows = rows <> transpose rows
```

The diagonal lines are trickier to generate.
We drop an increasing number of elements from the front of successive rows.
This generates the upper triangular matrix.
If we then transpose that triangular matrix, we get a set of diagonals.

To generate all the diagonals,
we need to do this to all combinations of reversals of rows and columns.
We also drop the main diagonals the second time they are generated.

```haskell
diags rows =
    tri rows <>
    tri (reverse rows) <>
    tail (tri (map reverse rows)) <>
    tail (tri (reverse $ map reverse rows))
  where
    tri = transpose . zipWith drop [0..]
```

Now we need the count for the word in each line.
The word can appear forwards or backwards.

```haskell
count word line =
    check word + check (reverse word)
  where
    check word = length $ filter (isPrefixOf word) $ tails line
```

# Part 2

We annotate each letter with its position in the grid.

```haskell
addPositions rows =
    zipWith addRow [0..] rows
  where
    addRow r row = first (r,) <$> zip [0..] row
```

Now we will look for occurances of the word in each line.
When we find one, we will return the position of the $k$-th letter.
As in part 1, the word can be backwards or forwards.

```haskell
findMiddles k word line =
    go word <> go (reverse word)
  where
    go word = map (fst.(!!k)) $ filter (isPrefixOf word . map snd) $ tails line
```

We need two such positions to match to have an "X".

```haskell
countXs = length . filter (==2) . map length . group . sort
```

Now we can count the "X-MAS" occurances.
Unlike in part 1, we only check the diagonal lines.

```haskell top:2
    let posRows = addPositions rows
    let posLines = diags posRows
    let middles = concatMap (findMiddles 1 "MAS") posLines
    print $ countXs middles
```

## Module header and imports

```haskell top
module Main where
```
