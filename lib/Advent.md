# Common Library for Advent of Code

This is a grab-bag of useful utilities.

```haskell
inBounds :: (IArray a e, Ix i) => a i e -> i -> Bool
inBounds = inRange . bounds
```

## Directions and Positions

Many of the Advent of Code problems require 2D reasoning.

```haskell top:1
type Pos = V2 Int
data Dir = N | E | S | W deriving (Eq,Ord,Enum,Bounded,Show)

allDirs = [N .. W]

cw W = N
cw d = succ d

ccw N = W
ccw d = pred d

opp N = S
opp S = N
opp E = W
opp W = E

step N = V2 (-1) 0
step S = V2 ( 1) 0
step W = V2 0 (-1)
step E = V2 0 ( 1)
```

## Read an array

This function takes two arguments: `mkLine` converts the input `String` into
`[a]`, where the `a` are the raw elements, and `mkElem` builds the actual
elements of the array.

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

It is common that we want the raw element to be the array element.

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
module Advent
    ( module Advent
    , module X
    , Map, Set
    ) where

import Control.Monad.ST    as X
import Data.Array.IArray   as X
import Data.Array.ST       as X
import Data.Array.Unboxed  as X
import Linear.V2           as X
import Control.Monad.State as X

import Data.Map.Strict ( Map )
import Data.Set        ( Set )

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
```