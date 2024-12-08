# [Day 7](https://adventofcode.com/2024/day/7)

We parse the input into lists of integers (the equations).

```haskell top:3
main = do
    let tr x = bool ' ' x $ isDigit x
    let parse = map (read @Int) . words . map tr
    equations <- map parse . lines <$> getContents
```

## Part 1

I feel an exponential blowup coming, but let's try just inserting the operators
and see what happens. This function checks to see whether an equation is
solvable by trying both possible operators for each additional argument.

```haskell
solvable (v:x:yy) =
    go x yy
  where
    go x [] = x == v
    go x (y:yy) = go (x+y) yy || go (x*y) yy
```

The answer is the sum of the values of the solvable equations.

```haskell top:3
    print $ sum $ map head $ filter solvable equations
```

## Part 2

The new concatenation operator. Textual concatenation of two decimal numbers is
like multiplying the first number by first power of ten greater than the second
number (to shift the first number over), and then adding the second number.

```haskell
ccat x y =
    x * m + y
  where
    (m:_) = dropWhile (<=y) $ iterate (*10) 1
```

We're just going to copy and paste the rest of Part 1.

```haskell
solvable2 (v:x:yy) =
    go x yy
  where
    go x [] = x == v
    go x (y:yy) = go (x+y) yy || go (x*y) yy || go (ccat x y) yy
```

The answer is the sum of the values of the solvable equations.

```haskell top:3
    print $ sum $ map head $ filter solvable2 equations
```

## Module header and imports

```haskell top
module Main where
```
