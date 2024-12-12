# [Day 12](https://adventofcode.com/2024/day/12)

Read the array of plants.

```haskell top:3
main = do
    plants <- getArray id (\_ _ x -> x) :: IO (UArray Pos Char)
```

From the plants we will find the regions. Each region will have an area and a
set of fences.

```haskell top:1
data Region = Region
    { _rArea :: !Int
    , _rFences :: !(Set Fence)
    } deriving Show
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

```haskell top:3
    let regions = mkRegions plants
    print $ sum . map part1 $ regions
    print $ sum . map part2 $ regions

dfsM :: Monad m => [a] -> (a -> m [a]) -> m ()
dfsM xx f = go xx where
    go [] = pure ()
    go (x:xx) = f x >>= go . (<>xx)

data Dir = N | E | S | W deriving (Eq,Ord,Show)

addToRegion fences Region{..} = Region
    (succ _rArea)
    (S.union (S.fromList $ mk <$> fences) _rFences)
  where mk (dir,pos) = Fence dir pos

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
            setVisited pos
            let (onmap,offmap) = ortho plants pos
            let (yesplant,noplant) = partition ((==c).(plants!).snd) onmap
            let next = map snd yesplant
            let edges = 4 - length next
            let fences = map (second $ const pos) $ offmap <> noplant
            modifySTRef' regions $ flip M.adjust k $ addToRegion fences
            pure next
    M.elems <$> readSTRef regions

part1 Region{..} = _rArea * S.size _rFences
part2 Region{..} = _rArea * countFences (S.toList _rFences)

countFences (start:more) = go start more where
    go _ [] = 1
    go (Fence dir pos) (next@(Fence dir' pos'):more)
        | dir == dir' && pos + bump dir == pos' = go next more
        | otherwise = 1 + go next more
    bump N = delta E
    bump S = delta E
    bump E = delta S
    bump W = delta S

delta N = V2 (-1) 0
delta S = V2 ( 1) 0
delta W = V2 0 (-1)
delta E = V2 0 ( 1)

deltas = ((,) <*> delta) <$> [N,E,S,W]
```

```haskell top:1
type Pos = V2 Int
type Ary a = Pos -> Maybe a
```

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

ortho ary pos = partition ((inRange $ bounds ary).snd) $ second (+pos) <$> deltas
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
