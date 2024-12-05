# [Day 5](https://adventofcode.com/2024/day/5)

For fun let's create a `newtype` for pages.

```haskell top:2
newtype Page = Page { _unPage :: Int } deriving (Eq,Ord,Show,Read)
```

The input has two parts. We treat each line as a list of `Page`s
and split the parts at the empty line.

```haskell top:3
main = do
    let tr c = bool ' ' c (isDigit c)
    let parse = map (Page . read @Int) . words . map tr
    let parts = second tail . span (not.null)
    (rules,updates) <- parts . map parse . lines <$> getContents
```

## Part 1

We'll encode the rules as a `Map Page (Set Page)`, storing the set of pages
that must come *after* each page.

```haskell top:1
type RulesMap = Map Page (Set Page)
```

```haskell
mkRulesAfter :: [[Page]] -> RulesMap
mkRulesAfter rules = M.fromListWith S.union $ mk <$> rules where
    mk [one,two] = (one, S.singleton two)
```

For each update we collect, at each position of the update, the set of pages
that come before that position.

```haskell
mkUpdateBefore :: [Page] -> [Set Page]
mkUpdateBefore update = scanl S.union S.empty $ S.singleton <$> update
```

An update is valid if, at each position, the set of pages that come before, and
the set of pages that must come after according to the rules, have a null
intersection.

```haskell
isValid :: RulesMap -> [Page] -> Bool
isValid rulesMap update =
    all S.null $ zipWith intersect update (mkUpdateBefore update)
  where
    intersect page beforeSet = S.intersection beforeSet $ getAfterSet page
    getAfterSet page = M.findWithDefault S.empty page rulesMap
```

The answer to part 1 is the sum of the middle elements of the valid updates.

```haskell top:3
    let middle pp = pp !! (length pp `div` 2)
    let rulesMap = mkRulesAfter rules
    print $ sum $ map (_unPage . middle) $ filter (isValid rulesMap) updates
```

## Module header and imports

```haskell top
module Main where
import Data.Map.Strict ( Map )
import Data.Set        ( Set )
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
```
