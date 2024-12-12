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
fences](#contiguous-fences).

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
    (S.union (S.fromList $ uncurry Fence <$> fences) _regionFences)
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
around the region.

Adjacent positions that are on the map and have the same plant are part of
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
add one to the count. A contiguous fence will be in the same direction, and
the position will be exactly one step away from the previous fence.

```haskell
countFences (S.toList -> (start:more)) = go start more where
    go _ [] = 1
    go (Fence dir pos) (next@(Fence dir' pos'):more)
        | dir == dir' && pos + walk dir == pos' = go next more
        | otherwise = 1 + go next more
```

If we are walking along a fence to the north or south, we are stepping east.
If we are walking along a fence to the east or west, we are stepping south.

```haskell
    walk N = step E
    walk S = step E
    walk E = step S
    walk W = step S
```

## Orthogonal neighbors

For this problem, we want to know the direction we took to each neighbor, and
we want to hold on to the offmap neighbors for the fence calcutions.

```haskell
ortho ary pos =
    partition inbounds neighbors
  where
    inbounds (_,pos) = inBounds ary pos
    neighbors = zip allDirs $ (+pos) . step <$> allDirs
```

## Module header and imports

```haskell top
module Main where
import Advent
import qualified Data.Map.Strict as M
import qualified Data.Set as S
```
