# Day 1

The input to this problem are two lists given pair by pair over `n` lines.

The first thing we'll do is read the integers in each line. This gives us a
list of lists of integers, which we transpose to get the two original lists.
Finally, we sort both lists.

```haskell
main :: IO ()
main = do
    let parse = map (read @Int) . words
    [aa,bb] <- map sort . transpose . map parse . lines <$> getContents
```

For Part 1, we sum the absolute difference between pairs of these two sorted
lists:

```haskell
    let part1 = sum $ zipWith (\a b -> abs (a-b)) aa bb
    print part1
```

For Part 2, we create a map (dictionary) of counts for each element of the
second list.  Then for each element of the first list, we multiply it by its
count, and sum those values.

```haskell
    let counts = M.fromListWith (+) $ zip bb (repeat 1)
        lookup k = M.findWithDefault 0 k counts
        part2 = sum $ aa <&> \a -> a * lookup a
    print part2
```

## Module header and imports

```haskell top
module Main where

import qualified Data.Map.Strict as M

