# [Day 8](https://adventofcode.com/2024/day/8)

Split the input into lines and get the bounds of the map.

```haskell top:3
main = do
    rows@(row:_) <- lines <$> getContents
    let nrows = length rows
    let ncols = length row
    let bounds = (1, V2 nrows ncols)
```

Find the positions of each emitter.

```haskell top:3
    let emitters = M.fromListWith (<>)
            [ (x, [V2 r c]) 
            | (r,row) <- zip [1..] rows
            , (c,x) <- zip [1..] row
            , x /= '.' ]
```

## Part 1

Each pair of emitters gives two antinodes.
We keep only those that are on the map.

```haskell
getAntinodes bounds positions = filter (inRange bounds) $ concat
    [ [2*(a-b)+b, 2*(b-a)+a]
    | (a:aa) <- tails positions
    , b <- aa ]
```

Find the positions of each emitter's antinodes.
Then put all the antinodes from all the emitters into a set.
The answer is the size of that set.

```haskell
solve get = S.size . S.fromList . concat . M.elems . M.map get
```

The answer to part 1.

```haskell top:3
    print $ solve (getAntinodes bounds) emitters
```

## Module header and imports

```haskell top
module Main where
import Linear.V2
import qualified Data.Map.Strict as M
import qualified Data.Set as S
```
