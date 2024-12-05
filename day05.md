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
    let (valid,invalid) = partition (isValid rulesMap) updates
    print $ sum $ map (_unPage . middle) valid
```

## Part 2

Now we need to repair the invalid updates and print the sum of their middle elements.

```haskell top:3
    print $ sum $ map (_unPage . middle) $ repair rulesMap <$> invalid
```

I'm not sure if it will be exercised by the input, but nothing in the problem description
prevents the rules from having cycles. It just prohibits cycles with respect to the pages
in any individual update.

So to repair the update, we first prune the rules to only those relevant to the update.

```haskell
pruneRules :: RulesMap -> [Page] -> RulesMap
pruneRules rulesMap update =
    M.restrictKeys rulesMap updateSet
        & M.map (S.intersection updateSet)
        & M.filter (not . S.null)
  where
    updateSet = S.fromList update
```

This pruned rules map will not have cycles.

Now we assign an ordering value to each key of this map that is one less than
the minimum ordering value of any of the pages that must come after that key.
Pages that have nothing that must come after them get the value zero (the
highest value). Two notes:

* The reason for the increasingly negative numbers is so that the final sort
  order will be non-decreasing.

* The computation of the value map is recursive and thus lazy, with the base
  case being the zero-valued pages.

```haskell
mkValueMap prunedMap =
    valueMap
  where
    valueMap = M.map (pred . minimum . map get . S.toList) prunedMap
    get k = M.findWithDefault 0 k valueMap
```

Now we just sort the update by each page's value.

```haskell
repair rulesMap update =
    sortBy (comparing get) update
  where
    valueMap = mkValueMap $ pruneRules rulesMap update
    get k = M.findWithDefault 0 k valueMap
```

## Module header and imports

```haskell top
module Main where
import Data.Map ( Map )
import Data.Set ( Set )
import qualified Data.Map as M
import qualified Data.Set as S
```
