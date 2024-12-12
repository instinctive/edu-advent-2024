# [Day 12](https://adventofcode.com/2024/day/12)

[Read the array](#read-an-array) of plants, [find the
regions](#find-the-regions), and print the answers.

```haskell top:3
main = do
    plants <- getArrayRaw id :: IO (UArray Pos Char)
    let regions = mkRegions plants
    print $ sum . map part1 $ regions
    print $ sum . map part2 $ regions
```

For part 1 we multiply the area by the raw number of fences.

```haskell
part1 Region{..} = _regionArea * S.size _regionFences
```

For part 2 we multiply the area by the [count of contiguous
fences](#contigouse-fences).

```haskell
part2 Region{..} = _regionArea * countFences _regionFences
```

## Regions and Fences

Each region will have an area and a set of fences.

```haskell top:1
data Region = Region
    { _regionArea :: !Int
    , _regionFences :: !(Set Fence)
    } deriving Show

emptyRegion = Region 0 S.empty
```

Each fence has a position that is inside the region, and a direction that
indicates which side of the position has the fence.  For part 2 we will count
contiguous runs of fences, so they will need to be in a sort order that puts
contiguous fences next to each other.

```haskell top:1
data Fence = Fence { _fenceDir :: !Dir, _fencePos :: !Pos } deriving (Eq,Show)

instance Ord Fence where
    compare (Fence d (V2 r c)) (Fence d' (V2 r' c')) =
        compare d d' <> case d of
            N -> compare r r' <> compare c c'
            S -> compare r r' <> compare c c'
            E -> compare c c' <> compare r r'
            W -> compare c c' <> compare r r'
```

When we add fences to a region, we also bump the area by one.

```haskell top:1
addFences fences Region{..} = Region
    (succ _regionArea)
    (S.union (S.fromList $ mk <$> fences) _regionFences)
  where mk (dir,pos) = Fence dir pos
```

## Find the Regions

This is where all the action is.

We create a new boolean array to tell us if we've visited a position, and then
from every unvisited position we create a new region $k$ for that plant $c$ and
do a [depth-first search](#depth-first-search) to find all the other positions
that comprise that region.

```haskell
mkRegions plants = runST do
    ary <- newArray (bounds plants) False :: ST s (STUArray s Pos Bool)
    let notVisited pos = not <$> readArray ary pos
    let setVisited pos = writeArray ary pos True
    regions <- newSTRef M.empty
    for_ (range $ bounds plants) \pos -> notVisited pos >>= flip when do
        let c = plants!pos
        k <- M.size <$> readSTRef regions
        modifySTRef' regions $ M.insert k $ Region 0 S.empty
        dfsM [pos] \pos -> notVisited pos >>= bool (pure []) do
```

Having found an unvisited position in region $k$, we mark it as visited and
then take a look in all the [orthogonal neighbors](#orthogonal-neighbors). Some
may be off the map, and some may not have the same plant. These will be fences
round the region, which we add to the region.

Adjacent positions that are on the map and have the same plant are also part of
this region, so we return them as the next positions to explore in the
depth-first search.

```haskell
            setVisited pos
            let (onmap,offmap) = ortho plants pos
            let (yesplant,noplant) = partition ((==c).(plants!).snd) onmap
            let next = map snd yesplant
            let fences = map (second $ const pos) $ offmap <> noplant
            modifySTRef' regions $ flip M.adjust k $ addFences fences
            pure next
```

When the depth-first search is done, we return the regions stored in the map.

```haskell
    M.elems <$> readSTRef regions
```

## Contiguous fences

We go down the list of fences. Whenever the next fence is not contiguous, we
add one the the count. A contiguous fence will be in the same direction, and
the position will be exactly one step away from the previous fence.

```haskell
countFences (S.toList -> (start:more)) = go start more where
    go _ [] = 1
    go (Fence dir pos) (next@(Fence dir' pos'):more)
        | dir == dir' && pos + step dir == pos' = go next more
        | otherwise = 1 + go next more
```

If we are walking alone a fence to the north or south, we are stepping east.
If we are walking alone a fence to the east or west, we are stepping south.

```haskell
    step N = delta E
    step S = delta E
    step E = delta S
    step W = delta S
```

## Directions and Positions

Many of the Advent of Code problems require 2D reasoning.

```haskell top:1
type Pos = V2 Int
data Dir = N | E | S | W deriving (Eq,Ord,Show)

delta N = V2 (-1) 0
delta S = V2 ( 1) 0
delta W = V2 0 (-1)
delta E = V2 0 ( 1)

deltas = ((,) <*> delta) <$> [N,E,S,W]
```

## Orthogonal neighbors

For this problem, we want to know the direction we took to each neighbor, and
we want to hold on to the offmap neighbors for the fence calcutions.

```haskell
ortho ary pos =
    partition ((inRange $ bounds ary).snd) $
    second (+pos) <$> deltas
```

## Read an array

This is common in Advent of Code. This function takes two arguments: `mkLine`
converts the input `String` into `[a]`, where the `a` are the raw elements, and
`mkElem` builds the actual elements of the array.

```haskell
getArray mkLine mkElem = do
    rows@(row:_) <- map mkLine . lines <$> getContents
    let nrows = length rows
    let ncols = length row
    let ary = listArray (1, V2 nrows ncols)
            [ mkElem ary (V2 r c) x
            | (r,row) <- zip [1..] rows
            , (c,x)   <- zip [1..] row ]
    pure ary
```

It is common that we just want the raw element to be the array element.

```haskell
getArrayRaw mkLine = getArray mkLine \_ _ x -> x
```

## Depth-first search

Depth-first search in a monadic context.

```haskell
dfsM :: Monad m => [a] -> (a -> m [a]) -> m ()
dfsM xx f = go xx where
    go [] = pure ()
    go (x:xx) = f x >>= go . (<>xx)
```

## Module header and imports

```haskell top
module Main where
import Control.Monad.ST
import Data.Array.IArray
import Data.Array.ST
import Data.Array.Unboxed
import Data.Map.Strict ( Map )
import Data.Set ( Set )
import Linear.V2
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Set as S
```
