# Day 2

The input to this problem are a number of reports, one report per line, and
each report is a list of integers:

```haskell top:2
main :: IO ()
main = do
    let parse = map (read @Int) . words
    reports <- map parse . lines <$> getContents
```

## Part 1

For Part 1, we check each report to see if it's safe, and print
the number of safe reports.
lists:

```haskell top:2
    let part1 = length $ filter isSafe reports
    print part1
```

A safe report, as defined in the problem statement, has levels that are either
strictly increasing or strictly decreasing, and the absolute difference between
successive levels `diff` is $0\lt\mathrm{diff}\lt 4$.

```haskell
isSafe xx@(a:b:_) | a > b = isSafe (map negate xx)
isSafe xx = all ok $ zipWith (-) (tail xx) xx
  where ok diff = 0 < diff && diff < 4
```

## Module header and imports

```haskell top
module Main where
