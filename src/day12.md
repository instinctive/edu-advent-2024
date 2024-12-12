# [Day 12](https://adventofcode.com/2024/day/12)

```haskell top:3
main = do
    plants <- getArray id (\_ _ x -> x) :: IO (UArray Pos Char)
    print $ solve plants

dfsM :: Monad m => (a -> m [a]) -> [a] -> m ()
dfsM f = go where
    go [] = pure ()
    go (x:xx) = f x >>= go . (<>xx)

solve plants = runST do
    ary <- newArray (bounds plants) False :: ST s (STUArray s Pos Bool)
    let notVisited pos = lift $ not <$> readArray ary pos
    let setVisited pos = lift $ writeArray ary pos True
    flip evalStateT M.empty do
        for_ (range $ bounds plants) \pos -> notVisited pos >>= flip when do
            let c = plants!pos
            k <- gets M.size
            modify' $ M.insert k (0,0)
            let quux pos = notVisited pos >>= bool (pure []) do
                    setVisited pos
                    let next = filter ((==c).(plants!)) $ map snd . fst $ ortho plants pos
                    let edges = 4 - length next
                    modify' $ M.adjust (bimap succ (+edges)) k
                    pure next
            dfsM quux [pos]
        sum . map (uncurry (*)) <$> gets M.elems
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

data Dir = N | E | S | W deriving (Eq,Ord,Show)

ortho ary pos = partition ((inRange $ bounds ary).snd) $ second (+pos) <$> deltas
  where
    deltas =
        [ (N, V2 (-1) 0)
        , (E, V2 0 ( 1))
        , (S, V2 ( 1) 0)
        , (W, V2 0 (-1)) ]
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
