module Lib
  ( -- * Synopsis
    -- $synopsis
    BoundedSet
  , getSet
  , singleton
  , demo
  ) where

import Data.Foldable (foldl')
import Data.Set (Set)
import qualified Data.Set as Set

newtype BoundedSet a = BoundedSet
  { unBoundedSet :: Int -> Set a
  }

getSet :: (Ord a) => BoundedSet a -> Int -> Set a
getSet boundedSet maxSize
  | maxSize < 1 = mempty
  | otherwise = unBoundedSet boundedSet maxSize

instance (Ord a) => Semigroup (BoundedSet a) where
  bs1 <> bs2 =
    BoundedSet $ \maxSize ->
      let s1 = unBoundedSet bs1 maxSize
          s2 = unBoundedSet bs2 maxSize
       in foldl' (go maxSize) s1 s2
    where
    go maxSize acc x
      | Set.size acc >= maxSize = acc
      | otherwise = Set.insert x acc

instance (Ord a) => Monoid (BoundedSet a) where
  mempty = BoundedSet $ const mempty

singleton :: a -> BoundedSet a
singleton x = BoundedSet $ const $ Set.singleton x

-- | Demo that can be run in @ghci@:
--
-- > λ> demo 0
-- > fromList ""
-- > λ> demo 1
-- > fromList "a"
-- > λ> demo 2
-- > fromList "ab"
-- > λ> demo 3
-- > fromList "abc"
demo :: Int -> IO ()
demo maxSize = print $ getSet boundedSet maxSize
  where
  boundedSet :: BoundedSet Char
  boundedSet = singleton 'a' <> singleton 'b' <> singleton 'c'

-- $synopsis
--
-- Often times, we need some data determined at runtime to drive our program's
-- behavior. When we need to thread configuration through our programs,
-- @ReaderT@ may come to mind. We can use the same idea behind @ReaderT@ in all
-- sorts of ways.
--
-- We demonstrate one example of this pattern by introducing a 'BoundedSet' data
-- type. This type wraps a 'Set', but has a config parameter capturing the max
-- size for the 'Set'. When the set is already at this max size, any additional
-- elements we attempt to add will be dropped.
--
-- This approach is particularly powerful considering that we are able to
-- leverage the max size in instance definitions (see the 'Semigroup' example),
-- even though the max size isn't concretely specified until runtime.
