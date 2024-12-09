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

Note the way this works: $a-b$ is the vector from $b$ to $a$. So then we add
twice that to $b$ to get the antinode on the other side of $a$. And likewise
the other way. There are other equivalent ways of doing the vector math.

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

## Part 2

The only difference here is the antinodes generator.

Whereas Part 1 used the vector from $a$ to $b$ (expressed as $a-b$), here we
need the minimum integral vector. For example, if $a-b$ is `V2 2 4`, we need to
use `V2 1 2` instead, or we will be skipping antinodes.

We get that minimum integral vector by dividing the row and column components
of $a-b$ by their greatest common divisor.

```haskell
getAntinodes2 bounds positions = filter (inRange bounds) $ concat
    [ get a b
    | (a:aa) <- tails positions
    , b <- aa ]
  where
    get a b =
        takeWhile (inRange bounds) (gen          <$> [0..]) <>
        takeWhile (inRange bounds) (gen . negate <$> [1..])
      where
        v@(V2 dr dc) = a - b
        dv = fmap (`div` gcd dr dc) v
        gen m = b + dv * V2 m m
```

The answer to part 2.

```haskell top:3
    print $ solve (getAntinodes2 bounds) emitters
```

## Module header and imports

```haskell top
module Main where
import Linear.V2
import qualified Data.Map.Strict as M
import qualified Data.Set as S
```
