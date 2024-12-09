# [Day 9](https://adventofcode.com/2024/day/9)

The input is a single string that we convert to a list of sizes.

```haskell top:3
main = do
    input <- map digitToInt <$> getLine
    print $ part1 input
    print $ part2 input
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
labelIDs = intersperse Nothing $ Just <$> [0..]
```

To compact the disk map, we continually transfer blocks from the back of the
disk to free space in the front. We do this by working with both the forward
disk map and the backward disk map. When this process meets in the middle we're
done.

```haskell
compact input =
    go (-1) fwd bwd
  where
    fwd = zip labelIDs input
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

## Part 2

This solution is not going to share much with Part 1.

As we move files around we will be altering the order of files and free space,
and changing the size of the free space. We'll represent both files and free
space as maps indexed by disk positions. This will make updates easy.
We'll hold the maps in the state monad.

The answer will be the sum of the score for each file in the final file map.

```haskell
part2 input =
    sum $ fileScore <$> M.assocs fileMap
  where
```

The file score is the sum of each block's position times the file id.

```haskell
    fileScore (pos,(id,size)) = sum $ take size $ (*id) <$> [pos..]
```

The final file map is created my making the initial maps and then
attempting to move every file from the back to free space in the front.

```haskell
    (fileMap,_) = flip execState (M.empty,M.empty) do
        mkInitialMaps input
        gets (reverse . M.assocs . fst) >>= traverse_ attempt
```

### The initial maps

We check each input block to see if it goes in the file map or the free
space map. Each map is indexed by the position of the block.

```haskell
mkInitialMaps input = 
    sequence_ $ zipWith3 mk labelIDs (scanl (+) 0 input) input
  where
    mk fileID pos size = case fileID of
        Nothing -> modify' (second $ M.insert pos size)
        Just id -> modify' (first  $ M.insert pos (id,size))
```

### The move attempts

We lookup a block of free space large enough to hold the file.
The position of that space must be closer to the front than
the file's posititon.

```haskell
attempt (oldpos, (id,filesize)) = do
    gets (find ((>=filesize).snd) . M.assocs . snd) >>= \case
        Nothing -> pure ()
        Just (newpos,freesize) -> when (newpos < oldpos) do
```

Now we can do the actual move.
We delete the free space block from its map.
Then me delete the file from its map and reinsert in the position
formerly held by the free space.
Then we check to see if there's any extra free space.
If there is, we insert it after the moved file.

```haskell
            modify' (second $ M.delete newpos)
            modify' (first $ M.delete oldpos)
            modify' (first $ M.insert newpos (id,filesize))
            let extrasize = freesize - filesize
            when (extrasize > 0) do
                let extrapos = newpos + filesize
                modify' (second $ M.insert extrapos extrasize)
```

## Module header and imports

```haskell top
module Main where
import Control.Monad.State ( execState, gets, modify' )
import Data.Map.Strict ( Map )
import Data.Set        ( Set )
import Data.Tagged
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
```
