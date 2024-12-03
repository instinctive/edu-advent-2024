# [Day 3](https://adventofcode.com/2024/day/3)

The input is the file contents.

```haskell top:2
main :: IO ()
main = do
    input <- getContents
```

We will do our calculations for part 1 and part 2 in the `State` monad,
whiwh will hold the sum of the `mul(X,Y)` values, initially zero.

```haskell top:1
type Calc v = State Int v
```

We run each part on the input.

```haskell top:2
    let go f = print $ flip execState 0 $ f input
    go part1
    go part2
```

## Part 1

Here we are looking for `mul(X,Y)` with some constraints on `X` and `Y`:
each is 1-3 digits. When we find one, it adds $x*y$ to the answer.

So we're looking for the magic strings `mul(`, `,`, and `)` and
the values of $x$ and $y$. If we run into a problem anywhere, we can restart
looking for the whole pattern.

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

Here there are two *states* we can be in:
*part2*, where we are searching for `mul(X,Y)` and `don't()`, and
*dont*, where we are searching for `do()`.

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
