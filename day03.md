# [Day 3](https://adventofcode.com/2024/day/3)

The input is the file contents.

```haskell top:2
main :: IO ()
main = do
    input <- getContents
```

## Part 1

Here we are looking for `mul(X,Y)` with some constraints on `X` and `Y`:
each is 1-3 digits. When we find one, it adds $x*y$ to the answer.

So we're looking for the magic strings `mul(`, `,`, and `)` and
the values of $x$ and $y$. If we run into a problem anywhere, we can restart
looking for the whole pattern.

First we search the string until we find a `mul(`:

```haskell
part1 "" = 0
part1 s
    | isPrefixOf "mul(" s = getMul part1 (drop 4 s)
    | otherwise = part1 (tail s)
```

Now we hope `s` has this form: `X,Y)...`, so we try to extract `X` and `Y`.
If something fails we continue from s, which has the initial `mul(` dropped,
so we guarantee progress through the entire string.

```haskell
getMul cont s
    | xlen < 1 || xlen > 3 = cont s -- check number of X digits
    | take 1 xmore /= ","  = cont s -- check for comma
    | ylen < 1 || ylen > 3 = cont s -- check number of Y digits
    | take 1 ymore /= ")"  = cont s -- check for right paren
    | otherwise = read @Int xraw * read @Int yraw + cont (tail ymore)
  where
    (xraw,xmore) = span isDigit s
    (yraw,ymore) = span isDigit (tail xmore)
    xlen = length xraw
    ylen = length yraw
```

We run `part1` on the whole input.

```haskell top:2
    print $ part1 input
```

## Part 2

Here there are two *states* we can be in:
*part2*, where we are searching for `mul(X,Y)` and `don't()`, and
*dont*, where we are searching for `do()`.

```haskell
part2 "" = 0
part2 s
    | isPrefixOf "don't()" s = dont (drop 7 s)
    | isPrefixOf "mul("    s = getMul part2 (drop 4 s)
    | otherwise = part2 (tail s)

dont "" = 0
dont s
    | isPrefixOf "do()" s = part2 (drop 4 s)
    | otherwise = dont (tail s)
```

We run `part2` on the whole input.

```haskell top:2
    print $ part2 input
```

## Module header and imports

```haskell top
module Main where
