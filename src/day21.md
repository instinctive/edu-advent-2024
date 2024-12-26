# [Day 21](https://adventofcode.com/2124/day/21)

```haskell
main = do
    n <- read @Int . head <$> getArgs
    i <- getContents
    let (scores,_) = solve n (lines i)
    print $ sum scores

solve' n ss = do
    let (v,m) = solve n ss
    traverse_ print $ M.assocs m
    pure v

solve :: Int -> [String] -> ([Int], Map (Int,String) Int)
solve n = flip runState M.empty . traverse (score n)

score n s = do
    len <- minimum <$> traverse (mexpanddir.(n,)) (expand numpad s)
    let num = read @Int $ filter isDigit s
    let value = len * num
    pure value

expand pad s =
    map concat $ sequence $
    zipWith go ('A':s) s
  where
    go :: Char -> Char -> [String]
    go a b = pad M.! (a,b) <&> (<> "A")

mexpanddir k@(n,s) = gets (M.lookup k) >>= \case
    Just n -> pure n
    Nothing | n == 0 -> update k (length s)
    Nothing -> sum <$> zipWithM f ('A':s) s >>= update k
  where
    f a b = minimum <$> traverse (mexpanddir.(n-1,)) (dirpad M.! (a,b) <&> (<> "A"))
    update k v = modify' (M.insert k v) >> pure v

minBy f =
    map snd .
    head .
    groupBy ((==) `on` fst) .
    sortBy (comparing fst) .
    map ((,).f <*> id)

numpadRaw = words "789 456 123 .0A"
dirpadRaw = words ".^A <v>"

numpad = mkPad numpadRaw
dirpad = mkPad dirpadRaw
--    & M.map head
--    & M.insert ('v','A') ">^"
--    & M.insert ('A','v') "v<"

toKey (V2 1 0)    = 'v'
toKey (V2 (-1) 0) = '^'
toKey (V2 0 1)    = '>'
toKey (V2 0 (-1)) = '<'

mkPad raw = vecmap where
    Just (_,death) = find ((=='.').fst) keypos
    nodeath pos vv = not $ any (==death) $ scanl (+) pos vv
    keypos :: [(Char,Pos)]
    keypos = [ (x, V2 r c)
        | (r,xx) <- zip [0..] raw
        , (c,x)  <- zip [0..] xx
        ]
    vecmap :: Map (Char,Char) [String]
    vecmap = M.fromList [ ((a,b), map toKey <$> cands)
        | (a,apos) <- keypos
        , a /= '.'
        , (b,bpos) <- keypos
        , b /= '.'
        , let V2 dr dc = bpos - apos
        , let vrow = replicate (abs dr) $ V2 (signum dr) 0
        , let vcol = replicate (abs dc) $ V2 0 (signum dc)
        , let cands | dr == 0 = [vcol]
                    | dc == 0 = [vrow]
                    | otherwise = [vrow <> vcol, vcol <> vrow] & filter (nodeath apos)
        -- , let moves = 
        --         replicate (abs dr) (V2 (signum dr) 0) <>
        --         replicate (abs dc) (V2 0 (signum dc))
        -- , let cands = permutations moves & filter (nodeath apos) . nub
        ]
```

```
    keymap :: Map (Char,Char) String
    keymap = M.map mk vecmap
    mk (V2 r c) | c >= 0    = cols c <> rows r
                | otherwise = rows r <> cols c
    cols c = rep c '<' '>'
    rows r = rep r '^' 'v'
    rep n neg pos = replicate (abs n) (bool neg pos $ n >= 0)
```

## Module header and imports

```haskell top
module Main where
import Advent
import qualified Data.Set as S
import qualified Data.Map.Strict as M
```
