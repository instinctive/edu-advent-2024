# [Day 9](https://adventofcode.com/2024/day/9)

The input is a single string that we convert to a list of integers.

```haskell top:3
main = do
    input <- map digitToInt <$> getLine
    print $ part1 input
```

## Part 1

The compacted disk map will be a sequence of file IDs.
We multiply each ID by its position and sum them.

```haskell
part1 = sum . zipWith (*) [0..] . compact
```

We label the input disk map with `Just id` for files and `Nothing` for free
space.

```haskell
labelIDs = zip (intersperse Nothing $ Just <$> [0..])
```

To compact the disk map, we continually transfer blocks from the back of the
disk to free space in the front. We do this by working with both the forward
disk map and the backward disk map. When this process meets in the middle we're
done.

```haskell
compact input =
    go (-1) fwd bwd
  where
    fwd = labelIDs input
    bwd = reverse fwd
```

The definition of `go` has several cases, so we'll comment on them individually.
Note that the first argument to `go` is the file ID of the last file that was
written going forwards.

- We ignore free space at the end of the disk map.

```haskell
    go q fwd ((Nothing,_):bwd) = go q fwd bwd
```

- If the end of the disk map has a file that we've already written going forwards,
  then we've wrapped around and we're done.

```haskell
    go q _ ((Just b,_):bwd) | b == q = []
```

- If we are looking at the same file both forwards and backwards, then we've met
  in the middle, and we're done after writing the rest of this file. This will be
  the portion that we see from the backwards list, since some of it might already
  have been written to free space.

```haskell
    go q ((Just a,_):fwd) ((Just b,m):bwd) | a == b = replicate m b
```

- If we are looking at a different file going forwards, we write it to disk.
  Note that we also carry that ID forward so we can tell if we've wrapped around.

```haskell
    go q ((Just a,n):fwd) bwd = replicate n a <> go a fwd bwd
```

- Finally, if there is free space going forward, we write as much of the
  backwards file as we can, which will be the minimum of the amount of free
  space and the size of the file.

```haskell
    go q ((Nothing,n):fwd) ((Just b,m):bwd) =
        replicate x b <> go q fwd' bwd'
      where
        x = min n m
        fwd' = if x == n then fwd else (Nothing,n-x):fwd
        bwd' = if x == m then bwd else (Just b,m-x):bwd
```

## Module header and imports

```haskell top
module Main where
```
