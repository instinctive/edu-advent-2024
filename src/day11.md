# [Day 11](https://adventofcode.com/2024/day/11)

The input is a list of integers (the "stones").

```haskell top:3
main = do
    input <- map (read @Int) . words <$> getContents
```

## Part 1

When we read that a stone can be split in two, that's the signal that the
calculation will be $O(2^n)$, where $n$ is the number of blinks. In other
words, the calculation will experience exponential blowup as $n$ gets large.

But for part 1 maybe we can get away with the simple solution,
and we'll build on it for the inevitable part 2.

The most interesting part of the blink is when a stone splits. We do this by
looking for the first power of 10 that is greater than the stone. If that power
is even, then the stone has an even number of digits, and we can split it by
taking quotient and remainder of the stone with respect to *half* that power.

```haskell
maybeSplit stone
    | even p = Just $ quotRem stone (10^(div p 2))
    | otherwise = Nothing
  where
    p = head $ dropWhile ((<=stone).(10^)) [1..]
```

*Observation*: We don't need to keep recreating the line of stones.
For each stone, we're only interested in how many stones it will become
over the course of $n$ blinks. So we can just count them.

Let us assume that we count *down* the number of blinks, $n$. Then
when $n=0$ the stone "becomes" one stone (itself).

```haskell
countStone (0,_) = 1
```

If $n>0$, then we need to recursively count the number of stones
after this blink, decrementing $n$.

```haskell
countStone (n,0) = countStone (n-1,1)
countStone (n,s) = case maybeSplit s of
    Nothing    -> countStone (n-1,s*2024)
    Just (q,r) -> countStone (n-1,q) + countStone (n-1,r)
```

The answer is just the sum of `countStone` on the input stones,
where we specify 25 blinks for each.

```haskell top:3
    let part1 = countStone . (25,)
    print $ sum $ map part1 input
```

## Part 2

It turns out the simple, exponential solution works fine for the 25 blinks of part 1 but
not the 75 blinks of part 2, as expected.

Our first instinct should be that we're probably repeating ourselves a lot in
this 75-layer binary tree.  What we'd like to do is just look up the solution
if we've already calculated it.  This calls for a memoized function.

```haskell
memo :: (MonadState (Map k v) m, Ord k) => (k -> m v) -> k -> m v
memo f k = gets (M.lookup k) >>= flip maybe pure do
    v <- f k
    modify' $ M.insert k v
    pure v
```

This function will try to look up the key $k$ in the map managed by the state
monad, or call $f k$ if it's not there to find it.

Now we need to rewrite `countStone` with the recursion factored out, and in
an applicative style. Compare this with the previous version!

```haskell
countStoneA _ (0,_) = pure 1
countStoneA f (n,0) = f (n-1,1)
countStoneA f (n,s) = case maybeSplit s of
    Nothing    -> f (n-1,s*2024)
    Just (q,r) -> (+) <$> f (n-1,q) <*> f (n-1,r)
```

The only differences are that the recursive calls are replaced with calls to $f$,
and the addition in the split case has been lifted into the applicative.

When we factor out recursion like this, we can get the recursive function
back using the
[fixpoint operator](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Function.html#v:fix) `fix`.

```haskell ignore
fix f = let x = f x in x
```

For example, `fix countStoneA` is almost equivalent to `countStone`,
except that it operates in an applicative context.
We can use the [identity monad](https://downloads.haskell.org/ghc/latest/docs/libraries/base-4.20.0.0-1f57/Data-Functor-Identity.html)
to supply a very simple such context,
and make the same (exponential) calculation of part 1.

```haskell top:3
    let part1' = runIdentity . fix countStoneA . (25,)
    print $ sum $ map part1' input
```

Now we can add the memoization to get the answer to part 2.

```haskell top:3
    let part2 = fix (memo . countStoneA) . (75,)
    print $ flip evalState M.empty $ sum <$> traverse part2 input
```

## Module header and imports

```haskell top
module Main where
import Control.Monad.State ( MonadState, State, evalState, gets, modify' )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
```

