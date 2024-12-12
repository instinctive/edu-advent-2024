# [Day 12](https://adventofcode.com/2024/day/12)

```haskell top:3
main = do
    plants <- getArray id (\_ _ x -> x) :: IO (UArray Pos Char)
    print $ solve plants

dfsM :: Monad m => (a -> m [a]) -> [a] -> m ()
dfsM f = go where
    go [] = pure ()
    go (x:xx) = f x >>= go . (<>xx)

data Dir = N | E | S | W deriving (Eq,Ord,Show)

data Region = Region
    { _rArea :: !Int
    , _rEdges :: !Int
    , _rFences :: ![ (Dir,Pos) ]
    } deriving Show

addRegion area edges fences Region{..} = Region
    (area + _rArea)
    (edges + _rEdges)
    (fences <> _rFences)

solve plants = runST do
    ary <- newArray (bounds plants) False :: ST s (STUArray s Pos Bool)
    let notVisited pos = lift $ not <$> readArray ary pos
    let setVisited pos = lift $ writeArray ary pos True
    flip evalStateT M.empty do
        for_ (range $ bounds plants) \pos -> notVisited pos >>= flip when do
            let c = plants!pos
            k <- gets M.size
            modify' $ M.insert k $ Region 0 0 []
            let quux pos = notVisited pos >>= bool (pure []) do
                    setVisited pos
                    let (onmap,offmap) = ortho plants pos
                    let (yesplant,noplant) = partition ((==c).(plants!).snd) onmap
                    let next = map snd yesplant
                    let edges = 4 - length next
                    let fences = map (second $ const pos) $ offmap <> noplant
                    modify' $ flip M.adjust k $ addRegion 1 edges fences
                    pure next
            dfsM quux [pos]
        regions <- gets M.elems
        pure
            ( sum . map part1 $ regions
            , sum . map part2 $ regions )

makeNW pos q@(dir,pos') = case dir of
    N -> (N,pos)
    W -> (W,pos)
    S -> (N,pos')
    E -> (W,pos')

part1 Region{..} = _rArea * _rEdges
part2 Region{..} = _rArea * n where
    n = countFences (sortBy (comparing fenceSort) _rFences)

fenceSort (N, V2 r c) = (N,r,c)
fenceSort (S, V2 r c) = (S,r,c)
fenceSort (W, V2 r c) = (W,c,r)
fenceSort (E, V2 r c) = (E,c,r)

countFences (start:more) = go [start] more where
    go :: [(Dir,Pos)] -> [(Dir,Pos)] -> Int
    go pp [] = 1
    go curr@((dir,pos):_) (next@(dir',pos'):more)
        | dir == dir' && pos + bump dir == pos' = go (next:curr) more
        | otherwise = 1 + go [next] more
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
import Linear.V2
import Control.Monad.State
import qualified Data.Map.Strict as M
```
