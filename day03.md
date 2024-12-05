# [Day 3](https://adventofcode.com/2024/day/3)

[I like the regex solution better](day03-re.md).

The input is the file contents.

```haskell top:2
main :: IO ()
main = do
    input <- getContents
```

We will do our calculations for part 1 and part 2 in the `State` monad,
which will hold the sum of the `mul(X,Y)` values.

```haskell top:1
type Calc v = State Int v
```

We run each part on the input string.

```haskell top:2
    let go f = print $ flip execState 0 $ f input
    go part1
    go part2
```

## Part 1

We're looking for the magic strings `mul(`, `,`, and `)` and
the values of $x$ and $y$.
First we search the string until we find a `mul(`:

```haskell
part1 :: String -> Calc ()
part1 "" = pure ()
part1 s
    | isPrefixOf "mul(" s = getMul (drop 4 s) >>= part1
    | otherwise = part1 (tail s)
```

Now we hope `s` has this form: `X,Y)...`, so we try to extract `X` and `Y`.
If something fails we continue from s, which has the initial `mul(` dropped,
so we guarantee progress through the entire string.

This function returns the rest of the string to search.

```haskell
getMul :: String -> Calc String
getMul s
    | xlen < 1 || xlen > 3 = pure s -- check number of X digits
    | take 1 xmore /= ","  = pure s -- check for comma
    | ylen < 1 || ylen > 3 = pure s -- check number of Y digits
    | take 1 ymore /= ")"  = pure s -- check for right paren
    | otherwise = modify' (+mulval) >> pure (tail ymore)
  where
    (xraw,xmore) = span isDigit s
    (yraw,ymore) = span isDigit (tail xmore)
    xlen = length xraw
    ylen = length yraw
    mulval = read @Int xraw * read @Int yraw
```

## Part 2

Here there are two states we can be in:
`part2`, where we are searching for `mul(X,Y)` and `don't()`, and
`dont`, where we are searching for `do()`.

```haskell
part2 :: String -> Calc ()
part2 "" = pure ()
part2 s
    | isPrefixOf "don't()" s = dont (drop 7 s)
    | isPrefixOf "mul("    s = getMul (drop 4 s) >>= part2
    | otherwise = part2 (tail s)

dont :: String -> Calc ()
dont "" = pure ()
dont s
    | isPrefixOf "do()" s = part2 (drop 4 s)
    | otherwise = dont (tail s)
```

## Module header and imports

```haskell top
module Main where
import Control.Monad.State ( State, execState, modify' )
```
