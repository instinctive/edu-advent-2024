# [Day 3 using regex](https://adventofcode.com/2024/day/3)

[I like this better than my original solution](day03.md).

I was taken by the elegance the regex-based solutions my friends did in Python.

To avoid any issues with multi-line regex matching,
we convert all newlines to spaces.

```haskell
unline '\n' = ' '
unline c = c
```

The input is the file contents, with newlines converted.

```haskell top:2
main :: IO ()
main = do
    input <- map unline <$> getContents
```

## Part 1

We're going to grab the `mul(X,Y)` entries with a regexp.

```haskell top:1
re_mul = [re|mul\((\d{1,3}),(\d{1,3})\)|]
```

We need to convert each match into an $x*y$ product.
(Note documentation of [`scan`](https://hackage.haskell.org/package/pcre-heavy-1.0.0.2/docs/Text-Regex-PCRE-Heavy.html#v:scan).)

```haskell
getMul (_,xy) = product $ read @Int <$> xy
```

The answer is the sum of these products.

```haskell top:2
    print $ sum $ getMul <$> scan re_mul input
```

## Part 2

The genius idea of my friend Pomme was to use `re.split(regex)`
to split the input on `don't()`/`do()` pairs.
Note that this needs to be a non-greedy match.

```haskell top:1
re_dontdo = [re|don't\(\).*?($|do\(\))|]
```

Now we just split the input and do `getMul` on all the pieces. Genius!

```haskell top:2
    let enabled = split re_dontdo input
    let matches = concat $ scan re_mul <$> enabled
    print $ sum $ getMul <$> matches
```

## Module header and imports

```haskell top
{-# LANGUAGE QuasiQuotes #-}
module Main where
import Text.Regex.PCRE.Heavy
```
