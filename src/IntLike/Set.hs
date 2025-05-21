{-# LANGUAGE CPP #-}

module IntLike.Set
  ( -- * Set type
    IntLikeSet (..)

    -- * Construction
  , empty
  , singleton
  , fromList
#if MIN_VERSION_containers(0,7,0)
  , fromRange
#endif
  , fromAscList
  , fromDistinctAscList

    -- * Insertion
  , insert

    -- * Deletion
  , delete

    -- * Generalized insertion/deletion
  , alterF

    -- * Query
  , member
  , notMember
  , lookupLT
  , lookupGT
  , lookupLE
  , lookupGE
  , null
  , size
  , isSubsetOf
  , isProperSubsetOf
  , disjoint

    -- * Combine
  , union
  , unions
  , difference
  , (\\)
  , intersection
#if MIN_VERSION_containers(0,8,0)
  , intersections
  , symmetricDifference
  -- , Intersection (..)
#endif

    -- * Filter
  , filter
  , partition
  , takeWhileAntitone
  , dropWhileAntitone
  , spanAntitone
  , split
  , splitMember
  , splitRoot

    -- * Map
  , map
  , mapMonotonic

    -- * Folds
  , foldr
  , foldl
#if MIN_VERSION_containers(0,8,0)
  , foldMap
#endif

    -- ** Strict folds
  , foldr'
  , foldl'

    -- ** Legacy folds
  , fold

    -- * Min\/Max

#if MIN_VERSION_containers(0,8,0)
  , lookupMin
  , lookupMax
#endif
  , findMin
  , findMax
  , deleteMin
  , deleteMax
  , deleteFindMin
  , deleteFindMax
  , maxView
  , minView

    -- * Conversion

    -- ** List
  , elems
  , toList
  , toAscList
  , toDescList

    -- * Extra
  , insertState
  , orderedPairs
  , unorderedPairs
  )
where

import Control.DeepSeq (NFData)
import Data.Coerce (Coercible, coerce)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
#if MIN_VERSION_containers(0,8,0)
import Data.List.NonEmpty (NonEmpty)
#endif
import Prelude hiding (filter, foldMap, foldl, foldr, map, null)

type role IntLikeSet nominal

newtype IntLikeSet x = IntLikeSet {unIntLikeSet :: IntSet}
  deriving stock (Show)
  deriving newtype (Eq, Ord, NFData, Semigroup, Monoid)

empty :: IntLikeSet x
empty = coerce IntSet.empty
{-# INLINE empty #-}

singleton :: (Coercible x Int) => x -> IntLikeSet x
singleton = coerce IntSet.singleton
{-# INLINE singleton #-}

fromList :: (Coercible x Int) => [x] -> IntLikeSet x
fromList = coerce IntSet.fromList
{-# INLINE fromList #-}

#if MIN_VERSION_containers(0,7,0)
fromRange :: (Coercible x Int) => (x, x) -> IntLikeSet x
fromRange = coerce IntSet.fromRange
#endif

fromAscList :: (Coercible x Int) => [x] -> IntLikeSet x
fromAscList = coerce IntSet.fromAscList

fromDistinctAscList :: (Coercible x Int) => [x] -> IntLikeSet x
fromDistinctAscList = coerce IntSet.fromDistinctAscList

insert :: (Coercible x Int) => x -> IntLikeSet x -> IntLikeSet x
insert = coerce IntSet.insert
{-# INLINE insert #-}

delete :: (Coercible x Int) => x -> IntLikeSet x -> IntLikeSet x
delete = coerce IntSet.delete
{-# INLINE delete #-}

alterF
  :: forall x f
   . (Coercible x Int)
  => (forall v u. (Coercible v u) => Coercible (f v) (f u))
  => (Functor f)
  => (Bool -> f Bool)
  -> x
  -> IntLikeSet x
  -> f (IntLikeSet x)
alterF = coerce (IntSet.alterF @f)
{-# INLINE alterF #-}

member :: (Coercible x Int) => x -> IntLikeSet x -> Bool
member = coerce IntSet.member
{-# INLINE member #-}

notMember :: (Coercible x Int) => x -> IntLikeSet x -> Bool
notMember = coerce IntSet.notMember
{-# INLINE notMember #-}

lookupLT :: (Coercible x Int) => x -> IntLikeSet x -> Maybe x
lookupLT = coerce IntSet.lookupLT
{-# INLINE lookupLT #-}

lookupGT :: (Coercible x Int) => x -> IntLikeSet x -> Maybe x
lookupGT = coerce IntSet.lookupGT
{-# INLINE lookupGT #-}

lookupLE :: (Coercible x Int) => x -> IntLikeSet x -> Maybe x
lookupLE = coerce IntSet.lookupLE
{-# INLINE lookupLE #-}

lookupGE :: (Coercible x Int) => x -> IntLikeSet x -> Maybe x
lookupGE = coerce IntSet.lookupGE
{-# INLINE lookupGE #-}

null :: IntLikeSet x -> Bool
null = coerce IntSet.null
{-# INLINE null #-}

size :: IntLikeSet x -> Int
size = coerce IntSet.size
{-# INLINE size #-}

isSubsetOf :: IntLikeSet x -> IntLikeSet x -> Bool
isSubsetOf = coerce IntSet.isSubsetOf
{-# INLINE isSubsetOf #-}

isProperSubsetOf :: IntLikeSet x -> IntLikeSet x -> Bool
isProperSubsetOf = coerce IntSet.isProperSubsetOf
{-# INLINE isProperSubsetOf #-}

disjoint :: IntLikeSet x -> IntLikeSet x -> Bool
disjoint = coerce IntSet.disjoint
{-# INLINE disjoint #-}

union :: IntLikeSet x -> IntLikeSet x -> IntLikeSet x
union = coerce IntSet.union
{-# INLINE union #-}

unions
  :: forall x f
   . (forall v u. (Coercible v u) => Coercible (f v) (f u))
  => (Foldable f)
  => f (IntLikeSet x)
  -> IntLikeSet x
unions = coerce (IntSet.unions @f)
{-# INLINE unions #-}

difference :: IntLikeSet x -> IntLikeSet x -> IntLikeSet x
difference = coerce IntSet.difference
{-# INLINE difference #-}

(\\) :: IntLikeSet x -> IntLikeSet x -> IntLikeSet x
(\\) = coerce (IntSet.\\)
{-# INLINE (\\) #-}

intersection :: IntLikeSet x -> IntLikeSet x -> IntLikeSet x
intersection = coerce IntSet.intersection
{-# INLINE intersection #-}

#if MIN_VERSION_containers(0,8,0)

intersections :: NonEmpty (IntLikeSet x) -> IntLikeSet x
intersections = coerce IntSet.intersections
{-# INLINE intersections #-}

symmetricDifference :: IntLikeSet x -> IntLikeSet x -> IntLikeSet x
symmetricDifference = coerce IntSet.symmetricDifference
{-# INLINE symmetricDifference #-}

newtype Intersection = Intersection {getIntersection :: IntSet}
   deriving stock (Show, Eq, Ord)
   deriving newtype Semigroup

#endif

filter :: (Coercible x Int) => (x -> Bool) -> IntLikeSet x -> IntLikeSet x
filter = coerce IntSet.filter
{-# INLINE filter #-}

partition :: (Coercible x Int) => (x -> Bool) -> IntLikeSet x -> (IntLikeSet x, IntLikeSet x)
partition = coerce IntSet.partition
{-# INLINE partition #-}

takeWhileAntitone :: (Coercible x Int) => (x -> Bool) -> IntLikeSet x -> IntLikeSet x
takeWhileAntitone = coerce IntSet.takeWhileAntitone
{-# INLINE takeWhileAntitone #-}

dropWhileAntitone :: (Coercible x Int) => (x -> Bool) -> IntLikeSet x -> IntLikeSet x
dropWhileAntitone = coerce IntSet.dropWhileAntitone
{-# INLINE dropWhileAntitone #-}

spanAntitone :: (Coercible x Int) => (x -> Bool) -> IntLikeSet x -> (IntLikeSet x, IntLikeSet x)
spanAntitone = coerce IntSet.spanAntitone
{-# INLINE spanAntitone #-}

split :: (Coercible x Int) => x -> IntLikeSet x -> (IntLikeSet x, IntLikeSet x)
split = coerce IntSet.split
{-# INLINE split #-}

splitMember :: (Coercible x Int) => x -> IntLikeSet x -> (IntLikeSet x, Bool, IntLikeSet x)
splitMember = coerce IntSet.splitMember
{-# INLINE splitMember #-}

splitRoot :: IntLikeSet x -> [IntLikeSet x]
splitRoot = coerce IntSet.splitRoot
{-# INLINE splitRoot #-}

map :: (Coercible x Int, Coercible y Int) => (x -> y) -> IntLikeSet x -> IntLikeSet y
map = coerce IntSet.map
{-# INLINE map #-}

mapMonotonic :: (Coercible x Int) => (x -> x) -> IntLikeSet x -> IntLikeSet x
mapMonotonic = coerce IntSet.mapMonotonic
{-# INLINE mapMonotonic #-}

foldr :: forall x b. (Coercible x Int) => (x -> b -> b) -> b -> IntLikeSet x -> b
foldr = coerce (IntSet.foldr @b)
{-# INLINE foldr #-}

foldl :: forall x a. (Coercible x Int) => (a -> x -> a) -> a -> IntLikeSet x -> a
foldl = coerce (IntSet.foldl @a)
{-# INLINE foldl #-}

#if MIN_VERSION_containers(0,8,0)
foldMap :: forall x a. (Coercible x Int) => (Monoid a) => (x -> a) -> IntLikeSet x -> a
foldMap = coerce (IntSet.foldMap @a)
{-# INLINE foldMap #-}
#endif

foldr' :: forall x b. (Coercible x Int) => (x -> b -> b) -> b -> IntLikeSet x -> b
foldr' = coerce (IntSet.foldr' @b)
{-# INLINE foldr' #-}

foldl' :: forall x a. (Coercible x Int) => (a -> x -> a) -> a -> IntLikeSet x -> a
foldl' = coerce (IntSet.foldl' @a)
{-# INLINE foldl' #-}

#if MIN_VERSION_containers(0,8,0)
-- Mirror the deprecation message from containers 0.8
{-# DEPRECATED fold "Use IntLike.Set.foldr instead" #-}
#endif

fold :: forall x b. (Coercible x Int) => (x -> b -> b) -> b -> IntLikeSet x -> b
fold = coerce (IntSet.fold @b)
{-# INLINE fold #-}

#if MIN_VERSION_containers(0,8,0)

lookupMin :: (Coercible x Int) => IntLikeSet x -> Maybe x
lookupMin = coerce (IntSet.lookupMin)
{-# INLINE lookupMin #-}

lookupMax :: (Coercible x Int) => IntLikeSet x -> Maybe x
lookupMax = coerce (IntSet.lookupMax)
{-# INLINE lookupMax #-}

#endif

findMin :: (Coercible x Int) => IntLikeSet x -> x
findMin = coerce IntSet.findMin
{-# INLINE findMin #-}

findMax :: (Coercible x Int) => IntLikeSet x -> x
findMax = coerce IntSet.findMax
{-# INLINE findMax #-}

deleteMin :: IntLikeSet x -> IntLikeSet x
deleteMin = coerce IntSet.deleteMin
{-# INLINE deleteMin #-}

deleteMax :: IntLikeSet x -> IntLikeSet x
deleteMax = coerce IntSet.deleteMax
{-# INLINE deleteMax #-}

deleteFindMin :: (Coercible x Int) => IntLikeSet x -> (x, IntLikeSet x)
deleteFindMin = coerce IntSet.deleteFindMin
{-# INLINE deleteFindMin #-}

deleteFindMax :: (Coercible x Int) => IntLikeSet x -> (x, IntLikeSet x)
deleteFindMax = coerce IntSet.deleteFindMax
{-# INLINE deleteFindMax #-}

maxView :: (Coercible x Int) => IntLikeSet x -> Maybe (x, IntLikeSet x)
maxView = coerce IntSet.maxView
{-# INLINE maxView #-}

minView :: (Coercible x Int) => IntLikeSet x -> Maybe (x, IntLikeSet x)
minView = coerce IntSet.minView
{-# INLINE minView #-}

elems :: (Coercible x Int) => IntLikeSet x -> [x]
elems = coerce IntSet.elems
{-# INLINE elems #-}

toList :: (Coercible x Int) => IntLikeSet x -> [x]
toList = coerce IntSet.toList
{-# INLINE toList #-}

toAscList :: (Coercible x Int) => IntLikeSet x -> [x]
toAscList = coerce IntSet.toAscList
{-# INLINE toAscList #-}

toDescList :: (Coercible x Int) => IntLikeSet x -> [x]
toDescList = coerce IntSet.toDescList
{-# INLINE toDescList #-}

-- Extras:

insertState :: (Coercible x Int) => (Bool -> b) -> x -> IntLikeSet x -> (b, IntLikeSet x)
insertState f x = coerce . IntSet.alterF (\b -> (f b, True)) (coerce x) . unIntLikeSet
{-# INLINE insertState #-}

orderedPairs :: (Coercible x Int) => IntLikeSet x -> [(x, x)]
orderedPairs s = let vs = toList s in [(x, y) | x <- vs, y <- vs]

unorderedPairs :: (Coercible x Int) => IntLikeSet x -> [(x, x)]
unorderedPairs = go1 . toList
 where
  go1 vs =
    case vs of
      [] -> []
      x : xs -> go2 x xs xs
  go2 x vl vs =
    case vl of
      [] -> go1 vs
      y : vl' -> (x, y) : go2 x vl' vs
