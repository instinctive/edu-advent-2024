# [Day 12](https://adventofcode.com/2024/day/12)

The input is groups of three lines per machine,
each line having two integers.
We turn each group into a list of six integers.

```haskell top:3
main = do
    input <- map stringToInts . lines . onlyDigits <$> getContents
    let prizes = concat <$> splitOn [[]] input
    print $ sum $ mapMaybe solve prizes
    print $ sum $ mapMaybe (solve . part2) prizes
```

For part2 we add a large number to the prize positions.

```haskell
part2 [ax,ay,bx,by,qx,qy] = [ax,ay,bx,by,qx+delta,qy+delta]
delta = 10000000000000
```

Each prize solution will have some number $na$ of A button presses,
and some number $nb$ of B button presses. A solution will satisfy
the following equations:

```math
na * ax + nb * bx = qx
```

```math
na * ay + nb * by = qy
```

Here we have two equations with two unknowns, so we can solve it:

```math
nb = {qy*ax-qx*ay}\over by*ax-bx*ay
na = {qx-nb*bx}\over ax
```

Here it is in code. Note that we use `quotRem` for the division,
and check to see that it is an integral divisor.

```haskell
solve [ax,ay,bx,by,qx,qy] =
    case quotRem qq bb of
        (nb,0) -> 
            let na = (qx - nb*bx) `div` ax in
            Just $ 3 * na + nb
        _ -> Nothing
  where
    qq = qy * ax - qx * ay
    bb = by * ax - bx * ay
```

## Module header and imports

```haskell top
module Main where
import Advent
import qualified Data.Map as M
import qualified Data.Set as S
```
