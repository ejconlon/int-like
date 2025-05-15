{-# LANGUAGE CPP #-}

module IntLike.Map
  ( -- * Map type
    IntLikeMap (..)

    -- * Construction
  , empty
  , singleton
  , fromSet

    -- ** From Unordered Lists
  , fromList
  , fromListWith
  , fromListWithKey

    -- ** From Ascending Lists
  , fromAscList
  , fromAscListWith
  , fromAscListWithKey
  , fromDistinctAscList

    -- * Insertion
  , insert
  , insertWith
  , insertWithKey
  , insertLookupWithKey

    -- * Deletion\/Update
  , delete
  , adjust
  , adjustWithKey
  , update
  , updateWithKey
  , updateLookupWithKey
  , alter
  , alterF

    -- * Query

    -- ** Lookup
  , lookup
  , (!?)
  , (!)
  , findWithDefault
  , member
  , notMember
  , lookupLT
  , lookupGT
  , lookupLE
  , lookupGE

    -- ** Size
  , null
  , size

    -- * Combine

    -- ** Union
  , union
  , unionWith
  , unionWithKey
  , unions
  , unionsWith

    -- ** Difference
  , difference
  , (\\)
  , differenceWith
  , differenceWithKey

    -- ** Intersection
  , intersection
  , intersectionWith
  , intersectionWithKey

    -- ** Symmetric difference
#if MIN_VERSION_containers(0,8,0)
  , symmetricDifference
#endif

    -- ** Disjoint
  , disjoint

    -- ** Compose
  , compose

    -- ** Universal combining function
  , mergeWithKey

    -- * Traversal

    -- ** Map
  , map
  , mapWithKey
  , traverseWithKey
  , traverseMaybeWithKey
  , mapAccum
  , mapAccumWithKey
  , mapAccumRWithKey
  , mapKeys
  , mapKeysWith
  , mapKeysMonotonic

    -- * Folds
  , foldr
  , foldl
  , foldrWithKey
  , foldlWithKey
  , foldMapWithKey

    -- ** Strict folds
  , foldr'
  , foldl'
  , foldrWithKey'
  , foldlWithKey'

    -- * Conversion
  , elems
  , keys
  , assocs
  , keysSet

    -- ** Lists
  , toList

    -- ** Ordered lists
  , toAscList
  , toDescList

    -- * Filter
  , filter
#if MIN_VERSION_containers(0,8,0)
  , filterKeys
#endif
  , filterWithKey
  , restrictKeys
  , withoutKeys
  , partition
  , partitionWithKey
  , takeWhileAntitone
  , dropWhileAntitone
  , spanAntitone
  , mapMaybe
  , mapMaybeWithKey
  , mapEither
  , mapEitherWithKey
  , split
  , splitLookup
  , splitRoot

    -- * Submap
  , isSubmapOf
  , isSubmapOfBy
  , isProperSubmapOf
  , isProperSubmapOfBy

    -- * Min\/Max
  , lookupMin
  , lookupMax
  , findMin
  , findMax
  , deleteMin
  , deleteMax
  , deleteFindMin
  , deleteFindMax
  , updateMin
  , updateMax
  , updateMinWithKey
  , updateMaxWithKey
  , minView
  , maxView
  , minViewWithKey
  , maxViewWithKey

    -- * Extra
  , partialLookup
  , insertState
  )
where

import Control.DeepSeq (NFData)
import Data.Coerce (Coercible, coerce)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import IntLike.Set (IntLikeSet (..))
import Prelude hiding (filter, foldl, foldr, lookup, map, null)

type role IntLikeMap nominal representational

newtype IntLikeMap x a = IntLikeMap {unIntLikeMap :: IntMap a}
  deriving stock (Show, Traversable)
  deriving newtype (Eq, Ord, Functor, Foldable, NFData, Semigroup, Monoid)

empty :: forall x a. IntLikeMap x a
empty = coerce (IntMap.empty @a)
{-# INLINE empty #-}

singleton :: forall x a. (Coercible x Int) => x -> a -> IntLikeMap x a
singleton = coerce (IntMap.singleton @a)
{-# INLINE singleton #-}

fromSet :: forall x a. (Coercible x Int) => (x -> a) -> IntLikeSet x -> IntLikeMap x a
fromSet = coerce (IntMap.fromSet @a)
{-# INLINE fromSet #-}

fromList :: forall x a. (Coercible x Int) => [(x, a)] -> IntLikeMap x a
fromList = coerce (IntMap.fromList @a)
{-# INLINE fromList #-}

fromListWith :: forall x a. (Coercible x Int) => (a -> a -> a) -> [(x, a)] -> IntLikeMap x a
fromListWith = coerce (IntMap.fromListWith @a)
{-# INLINE fromListWith #-}

fromListWithKey :: forall x a. (Coercible x Int) => (x -> a -> a -> a) -> [(x, a)] -> IntLikeMap x a
fromListWithKey = coerce (IntMap.fromListWithKey @a)
{-# INLINE fromListWithKey #-}

fromAscList :: forall x a. (Coercible x Int) => [(x, a)] -> IntLikeMap x a
fromAscList = coerce (IntMap.fromAscList @a)
{-# INLINE fromAscList #-}

fromAscListWith :: forall x a. (Coercible x Int) => (a -> a -> a) -> [(x, a)] -> IntLikeMap x a
fromAscListWith = coerce (IntMap.fromAscListWith @a)
{-# INLINE fromAscListWith #-}

fromAscListWithKey :: forall x a. (Coercible x Int) => (x -> a -> a -> a) -> [(x, a)] -> IntLikeMap x a
fromAscListWithKey = coerce (IntMap.fromAscListWithKey @a)
{-# INLINE fromAscListWithKey #-}

fromDistinctAscList :: forall x a. (Coercible x Int) => [(x, a)] -> IntLikeMap x a
fromDistinctAscList = coerce (IntMap.fromDistinctAscList @a)
{-# INLINE fromDistinctAscList #-}

insert :: forall x a. (Coercible x Int) => x -> a -> IntLikeMap x a -> IntLikeMap x a
insert = coerce (IntMap.insert @a)
{-# INLINE insert #-}

insertWith :: forall x a. (Coercible x Int) => (a -> a -> a) -> x -> a -> IntLikeMap x a -> IntLikeMap x a
insertWith = coerce (IntMap.insertWith @a)
{-# INLINE insertWith #-}

insertWithKey :: forall x a. (Coercible x Int) => (x -> a -> a -> a) -> x -> a -> IntLikeMap x a -> IntLikeMap x a
insertWithKey = coerce (IntMap.insertWithKey @a)
{-# INLINE insertWithKey #-}

insertLookupWithKey
  :: forall x a. (Coercible x Int) => (x -> a -> a -> a) -> x -> a -> IntLikeMap x a -> (Maybe a, IntLikeMap x a)
insertLookupWithKey = coerce (IntMap.insertLookupWithKey @a)
{-# INLINE insertLookupWithKey #-}

delete :: forall x a. (Coercible x Int) => x -> IntLikeMap x a -> IntLikeMap x a
delete = coerce (IntMap.delete @a)
{-# INLINE delete #-}

adjust :: forall x a. (Coercible x Int) => (a -> a) -> x -> IntLikeMap x a -> IntLikeMap x a
adjust = coerce (IntMap.adjust @a)
{-# INLINE adjust #-}

adjustWithKey :: forall x a. (Coercible x Int) => (x -> a -> a) -> x -> IntLikeMap x a -> IntLikeMap x a
adjustWithKey = coerce (IntMap.adjustWithKey @a)
{-# INLINE adjustWithKey #-}

update :: forall x a. (Coercible x Int) => (a -> Maybe a) -> x -> IntLikeMap x a -> IntLikeMap x a
update = coerce (IntMap.update @a)
{-# INLINE update #-}

updateWithKey :: forall x a. (Coercible x Int) => (x -> a -> Maybe a) -> x -> IntLikeMap x a -> IntLikeMap x a
updateWithKey = coerce (IntMap.updateWithKey @a)
{-# INLINE updateWithKey #-}

updateLookupWithKey
  :: forall x a. (Coercible x Int) => (x -> a -> Maybe a) -> x -> IntLikeMap x a -> (Maybe a, IntLikeMap x a)
updateLookupWithKey = coerce (IntMap.updateLookupWithKey @a)
{-# INLINE updateLookupWithKey #-}

alter :: forall x a. (Coercible x Int) => (Maybe a -> Maybe a) -> x -> IntLikeMap x a -> IntLikeMap x a
alter = coerce (IntMap.alter @a)
{-# INLINE alter #-}

alterF
  :: forall x f a
   . (Coercible x Int)
  => (forall v u. (Coercible v u) => Coercible (f v) (f u))
  => (Functor f)
  => (Maybe a -> f (Maybe a))
  -> x
  -> IntLikeMap x a
  -> f (IntLikeMap x a)
alterF = coerce (IntMap.alterF @f @a)
{-# INLINE alterF #-}

lookup :: forall x a. (Coercible x Int) => x -> IntLikeMap x a -> Maybe a
lookup = coerce (IntMap.lookup @a)
{-# INLINE lookup #-}

(!?) :: forall x a. (Coercible x Int) => IntLikeMap x a -> x -> Maybe a
(!?) = coerce ((IntMap.!?) @a)
{-# INLINE (!?) #-}

(!) :: forall x a. (Coercible x Int) => IntLikeMap x a -> x -> a
(!) = coerce ((IntMap.!) @a)
{-# INLINE (!) #-}

findWithDefault :: forall x a. (Coercible x Int) => a -> x -> IntLikeMap x a -> a
findWithDefault = coerce (IntMap.findWithDefault @a)
{-# INLINE findWithDefault #-}

member :: forall x a. (Coercible x Int) => x -> IntLikeMap x a -> Bool
member = coerce (IntMap.member @a)
{-# INLINE member #-}

notMember :: forall x a. (Coercible x Int) => x -> IntLikeMap x a -> Bool
notMember = coerce (IntMap.notMember @a)
{-# INLINE notMember #-}

lookupLT :: forall x a. (Coercible x Int) => x -> IntLikeMap x a -> Maybe (x, a)
lookupLT = coerce (IntMap.lookupLT @a)
{-# INLINE lookupLT #-}

lookupGT :: forall x a. (Coercible x Int) => x -> IntLikeMap x a -> Maybe (x, a)
lookupGT = coerce (IntMap.lookupGT @a)
{-# INLINE lookupGT #-}

lookupLE :: forall x a. (Coercible x Int) => x -> IntLikeMap x a -> Maybe (x, a)
lookupLE = coerce (IntMap.lookupLE @a)
{-# INLINE lookupLE #-}

lookupGE :: forall x a. (Coercible x Int) => x -> IntLikeMap x a -> Maybe (x, a)
lookupGE = coerce (IntMap.lookupGE @a)
{-# INLINE lookupGE #-}

null :: forall x a. IntLikeMap x a -> Bool
null = coerce (IntMap.null @a)
{-# INLINE null #-}

size :: forall x a. IntLikeMap x a -> Int
size = coerce (IntMap.size @a)
{-# INLINE size #-}

union :: forall x a. IntLikeMap x a -> IntLikeMap x a -> IntLikeMap x a
union = coerce (IntMap.union @a)
{-# INLINE union #-}

unionWith :: forall x a. (a -> a -> a) -> IntLikeMap x a -> IntLikeMap x a -> IntLikeMap x a
unionWith = coerce (IntMap.unionWith @a)
{-# INLINE unionWith #-}

unionWithKey
  :: forall x a. (Coercible x Int) => (x -> a -> a -> a) -> IntLikeMap x a -> IntLikeMap x a -> IntLikeMap x a
unionWithKey = coerce (IntMap.unionWithKey @a)
{-# INLINE unionWithKey #-}

unions
  :: forall x f a
   . (forall v u. (Coercible v u) => Coercible (f v) (f u))
  => (Foldable f)
  => f (IntLikeMap x a)
  -> IntLikeMap x a
unions = coerce (IntMap.unions @f @a)
{-# INLINE unions #-}

unionsWith
  :: forall x f a
   . (forall v u. (Coercible v u) => Coercible (f v) (f u))
  => (Foldable f)
  => (a -> a -> a)
  -> f (IntLikeMap x a)
  -> IntLikeMap x a
unionsWith = coerce (IntMap.unionsWith @f @a)
{-# INLINE unionsWith #-}

difference :: forall x a b. IntLikeMap x a -> IntLikeMap x b -> IntLikeMap x a
difference = coerce (IntMap.difference @a @b)
{-# INLINE difference #-}

(\\) :: forall x a b. IntLikeMap x a -> IntLikeMap x b -> IntLikeMap x a
(\\) = coerce ((IntMap.\\) @a @b)
{-# INLINE (\\) #-}

differenceWith
  :: forall x a b. (a -> b -> Maybe a) -> IntLikeMap x a -> IntLikeMap x b -> IntLikeMap x a
differenceWith = coerce (IntMap.differenceWith @a @b)
{-# INLINE differenceWith #-}

differenceWithKey
  :: forall x a b. (Coercible x Int) => (x -> a -> b -> Maybe a) -> IntLikeMap x a -> IntLikeMap x b -> IntLikeMap x a
differenceWithKey = coerce (IntMap.differenceWithKey @a @b)
{-# INLINE differenceWithKey #-}

intersection :: forall x a b. IntLikeMap x a -> IntLikeMap x b -> IntLikeMap x a
intersection = coerce (IntMap.intersection @a @b)
{-# INLINE intersection #-}

intersectionWith :: forall x a b c. (a -> b -> c) -> IntLikeMap x a -> IntLikeMap x b -> IntLikeMap x c
intersectionWith = coerce (IntMap.intersectionWith @a @b @c)
{-# INLINE intersectionWith #-}

intersectionWithKey
  :: forall x a b c. (Coercible x Int) => (x -> a -> b -> c) -> IntLikeMap x a -> IntLikeMap x b -> IntLikeMap x c
intersectionWithKey = coerce (IntMap.intersectionWithKey @a @b @c)
{-# INLINE intersectionWithKey #-}

#if MIN_VERSION_containers(0,8,0)
symmetricDifference :: forall x a. IntLikeMap x a -> IntLikeMap x a -> IntLikeMap x a
symmetricDifference = coerce (IntMap.symmetricDifference @a)
{-# INLINE symmetricDifference #-}
#endif

disjoint :: forall x a b. IntLikeMap x a -> IntLikeMap x b -> Bool
disjoint = coerce (IntMap.disjoint @a @b)
{-# INLINE disjoint #-}

compose :: forall x c. IntLikeMap x c -> IntMap Int -> IntLikeMap x c
compose = coerce (IntMap.compose @c)
{-# INLINE compose #-}

mergeWithKey
  :: forall x a b c
   . (Coercible x Int)
  => (x -> a -> b -> Maybe c)
  -> (IntLikeMap x a -> IntLikeMap x c)
  -> (IntLikeMap x b -> IntLikeMap x c)
  -> IntLikeMap x a
  -> IntLikeMap x b
  -> IntLikeMap x c
mergeWithKey = coerce (IntMap.mergeWithKey @a @b @c)
{-# INLINE mergeWithKey #-}

map :: forall x a b. (a -> b) -> IntLikeMap x a -> IntLikeMap x b
map = coerce (IntMap.map @a @b)
{-# INLINE map #-}

mapWithKey :: forall x a b. (Coercible x Int) => (x -> a -> b) -> IntLikeMap x a -> IntLikeMap x b
mapWithKey = coerce (IntMap.mapWithKey @a @b)
{-# INLINE mapWithKey #-}

traverseWithKey
  :: forall x t a b
   . (Coercible x Int)
  => (forall v u. (Coercible v u) => Coercible (t v) (t u))
  => (Applicative t)
  => (x -> a -> t b)
  -> IntLikeMap x a
  -> t (IntLikeMap x b)
traverseWithKey = coerce (IntMap.traverseWithKey @t @a @b)
{-# INLINE traverseWithKey #-}

traverseMaybeWithKey
  :: forall x f a b
   . (Coercible x Int)
  => (forall v u. (Coercible v u) => Coercible (f v) (f u))
  => (Applicative f)
  => (x -> a -> f (Maybe b))
  -> IntLikeMap x a
  -> f (IntLikeMap x b)
traverseMaybeWithKey = coerce (IntMap.traverseMaybeWithKey @f @a @b)
{-# INLINE traverseMaybeWithKey #-}

mapAccum :: forall x a b c. (a -> b -> (a, c)) -> a -> IntLikeMap x b -> (a, IntLikeMap x c)
mapAccum = coerce (IntMap.mapAccum @a @b @c)
{-# INLINE mapAccum #-}

mapAccumWithKey
  :: forall x a b c. (Coercible x Int) => (a -> x -> b -> (a, c)) -> a -> IntLikeMap x b -> (a, IntLikeMap x c)
mapAccumWithKey = coerce (IntMap.mapAccumWithKey @a @b @c)
{-# INLINE mapAccumWithKey #-}

mapAccumRWithKey
  :: forall x a b c. (Coercible x Int) => (a -> x -> b -> (a, c)) -> a -> IntLikeMap x b -> (a, IntLikeMap x c)
mapAccumRWithKey = coerce (IntMap.mapAccumRWithKey @a @b @c)
{-# INLINE mapAccumRWithKey #-}

mapKeys :: forall x a. (Coercible x Int) => (x -> x) -> IntLikeMap x a -> IntLikeMap x a
mapKeys = coerce (IntMap.mapKeys @a)
{-# INLINE mapKeys #-}

mapKeysWith :: forall x a. (Coercible x Int) => (a -> a -> a) -> (x -> x) -> IntLikeMap x a -> IntLikeMap x a
mapKeysWith = coerce (IntMap.mapKeysWith @a)
{-# INLINE mapKeysWith #-}

mapKeysMonotonic :: forall x a. (Coercible x Int) => (x -> x) -> IntLikeMap x a -> IntLikeMap x a
mapKeysMonotonic = coerce (IntMap.mapKeysMonotonic @a)
{-# INLINE mapKeysMonotonic #-}

foldr :: forall x a b. (a -> b -> b) -> b -> IntLikeMap x a -> b
foldr = coerce (IntMap.foldr @a @b)
{-# INLINE foldr #-}

foldl :: forall x a b. (a -> b -> a) -> a -> IntLikeMap x b -> a
foldl = coerce (IntMap.foldl @a @b)
{-# INLINE foldl #-}

foldrWithKey :: forall x a b. (Coercible x Int) => (x -> a -> b -> b) -> b -> IntLikeMap x a -> b
foldrWithKey = coerce (IntMap.foldrWithKey @a @b)
{-# INLINE foldrWithKey #-}

foldlWithKey :: forall x a b. (Coercible x Int) => (a -> x -> b -> a) -> a -> IntLikeMap x b -> a
foldlWithKey = coerce (IntMap.foldlWithKey @a @b)
{-# INLINE foldlWithKey #-}

foldMapWithKey :: forall x m a. (Coercible x Int) => (Monoid m) => (x -> a -> m) -> IntLikeMap x a -> m
foldMapWithKey = coerce (IntMap.foldMapWithKey @m @a)
{-# INLINE foldMapWithKey #-}

foldr' :: forall x a b. (a -> b -> b) -> b -> IntLikeMap x a -> b
foldr' = coerce (IntMap.foldr' @a @b)
{-# INLINE foldr' #-}

foldl' :: forall x a b. (a -> b -> a) -> a -> IntLikeMap x b -> a
foldl' = coerce (IntMap.foldl' @a @b)
{-# INLINE foldl' #-}

foldrWithKey' :: forall x a b. (Coercible x Int) => (x -> a -> b -> b) -> b -> IntLikeMap x a -> b
foldrWithKey' = coerce (IntMap.foldrWithKey' @a @b)
{-# INLINE foldrWithKey' #-}

foldlWithKey' :: forall x a b. (Coercible x Int) => (a -> x -> b -> a) -> a -> IntLikeMap x b -> a
foldlWithKey' = coerce (IntMap.foldlWithKey' @a @b)
{-# INLINE foldlWithKey' #-}

elems :: forall x a. IntLikeMap x a -> [a]
elems = coerce (IntMap.elems @a)
{-# INLINE elems #-}

keys :: forall x a. (Coercible x Int) => IntLikeMap x a -> [x]
keys = coerce (IntMap.keys @a)
{-# INLINE keys #-}

assocs :: forall x a. (Coercible x Int) => IntLikeMap x a -> [(x, a)]
assocs = coerce (IntMap.assocs @a)
{-# INLINE assocs #-}

keysSet :: forall x a. IntLikeMap x a -> IntLikeSet x
keysSet = coerce (IntMap.keysSet @a)
{-# INLINE keysSet #-}

toList :: forall x a. (Coercible x Int) => IntLikeMap x a -> [(x, a)]
toList = coerce (IntMap.toList @a)
{-# INLINE toList #-}

toAscList :: forall x a. (Coercible x Int) => IntLikeMap x a -> [(x, a)]
toAscList = coerce (IntMap.toAscList @a)
{-# INLINE toAscList #-}

toDescList :: forall x a. (Coercible x Int) => IntLikeMap x a -> [(x, a)]
toDescList = coerce (IntMap.toDescList @a)
{-# INLINE toDescList #-}

filter :: forall x a. (a -> Bool) -> IntLikeMap x a -> IntLikeMap x a
filter = coerce (IntMap.filter @a)
{-# INLINE filter #-}

#if MIN_VERSION_containers(0,8,0)
filterKeys :: forall x a. (Coercible x Int) => (x -> Bool) -> IntLikeMap x a -> IntLikeMap x a
filterKeys = coerce (IntMap.filterKeys @a)
{-# INLINE filterKeys #-}
#endif

filterWithKey :: forall x a. (Coercible x Int) => (x -> a -> Bool) -> IntLikeMap x a -> IntLikeMap x a
filterWithKey = coerce (IntMap.filterWithKey @a)
{-# INLINE filterWithKey #-}

restrictKeys :: forall x a. IntLikeMap x a -> IntLikeSet x -> IntLikeMap x a
restrictKeys = coerce (IntMap.restrictKeys @a)
{-# INLINE restrictKeys #-}

withoutKeys :: forall x a. IntLikeMap x a -> IntLikeSet x -> IntLikeMap x a
withoutKeys = coerce (IntMap.withoutKeys @a)
{-# INLINE withoutKeys #-}

partition :: forall x a. (a -> Bool) -> IntLikeMap x a -> (IntLikeMap x a, IntLikeMap x a)
partition = coerce (IntMap.partition @a)
{-# INLINE partition #-}

partitionWithKey
  :: forall x a. (Coercible x Int) => (x -> a -> Bool) -> IntLikeMap x a -> (IntLikeMap x a, IntLikeMap x a)
partitionWithKey = coerce (IntMap.partitionWithKey @a)
{-# INLINE partitionWithKey #-}

takeWhileAntitone :: forall x a. (Coercible x Int) => (x -> Bool) -> IntLikeMap x a -> IntLikeMap x a
takeWhileAntitone = coerce (IntMap.takeWhileAntitone @a)
{-# INLINE takeWhileAntitone #-}

dropWhileAntitone :: forall x a. (Coercible x Int) => (x -> Bool) -> IntLikeMap x a -> IntLikeMap x a
dropWhileAntitone = coerce (IntMap.dropWhileAntitone @a)
{-# INLINE dropWhileAntitone #-}

spanAntitone :: forall x a. (Coercible x Int) => (x -> Bool) -> IntLikeMap x a -> (IntLikeMap x a, IntLikeMap x a)
spanAntitone = coerce (IntMap.spanAntitone @a)
{-# INLINE spanAntitone #-}

mapMaybe :: forall x a b. (a -> Maybe b) -> IntLikeMap x a -> IntLikeMap x b
mapMaybe = coerce (IntMap.mapMaybe @a @b)
{-# INLINE mapMaybe #-}

mapMaybeWithKey :: forall x a b. (Coercible x Int) => (x -> a -> Maybe b) -> IntLikeMap x a -> IntLikeMap x b
mapMaybeWithKey = coerce (IntMap.mapMaybeWithKey @a @b)
{-# INLINE mapMaybeWithKey #-}

mapEither
  :: forall x a b c. (a -> Either b c) -> IntLikeMap x a -> (IntLikeMap x b, IntLikeMap x c)
mapEither = coerce (IntMap.mapEither @a @b @c)
{-# INLINE mapEither #-}

mapEitherWithKey
  :: forall x a b c. (Coercible x Int) => (x -> a -> Either b c) -> IntLikeMap x a -> (IntLikeMap x b, IntLikeMap x c)
mapEitherWithKey = coerce (IntMap.mapEitherWithKey @a @b @c)
{-# INLINE mapEitherWithKey #-}

split :: forall x a. (Coercible x Int) => x -> IntLikeMap x a -> (IntLikeMap x a, IntLikeMap x a)
split = coerce (IntMap.split @a)
{-# INLINE split #-}

splitLookup :: forall x a. (Coercible x Int) => x -> IntLikeMap x a -> (IntLikeMap x a, Maybe a, IntLikeMap x a)
splitLookup = coerce (IntMap.splitLookup @a)
{-# INLINE splitLookup #-}

splitRoot :: forall x a. IntLikeMap x a -> [IntLikeMap x a]
splitRoot = coerce (IntMap.splitRoot @a)
{-# INLINE splitRoot #-}

isSubmapOf :: forall x a. (Eq a) => IntLikeMap x a -> IntLikeMap x a -> Bool
isSubmapOf = coerce (IntMap.isSubmapOf @a)
{-# INLINE isSubmapOf #-}

isSubmapOfBy :: forall x a b. (a -> b -> Bool) -> IntLikeMap x a -> IntLikeMap x b -> Bool
isSubmapOfBy = coerce (IntMap.isSubmapOfBy @a @b)
{-# INLINE isSubmapOfBy #-}

isProperSubmapOf :: forall x a. (Eq a) => IntLikeMap x a -> IntLikeMap x a -> Bool
isProperSubmapOf = coerce (IntMap.isProperSubmapOf @a)
{-# INLINE isProperSubmapOf #-}

isProperSubmapOfBy :: forall x a b. (a -> b -> Bool) -> IntLikeMap x a -> IntLikeMap x b -> Bool
isProperSubmapOfBy = coerce (IntMap.isProperSubmapOfBy @a @b)
{-# INLINE isProperSubmapOfBy #-}

lookupMin :: forall x a. (Coercible x Int) => IntLikeMap x a -> Maybe (x, a)
lookupMin = coerce (IntMap.lookupMin @a)
{-# INLINE lookupMin #-}

lookupMax :: forall x a. (Coercible x Int) => IntLikeMap x a -> Maybe (x, a)
lookupMax = coerce (IntMap.lookupMax @a)
{-# INLINE lookupMax #-}

findMin :: forall x a. (Coercible x Int) => IntLikeMap x a -> (x, a)
findMin = coerce (IntMap.findMin @a)
{-# INLINE findMin #-}

findMax :: forall x a. (Coercible x Int) => IntLikeMap x a -> (x, a)
findMax = coerce (IntMap.findMax @a)
{-# INLINE findMax #-}

deleteMin :: forall x a. IntLikeMap x a -> IntLikeMap x a
deleteMin = coerce (IntMap.deleteMin @a)
{-# INLINE deleteMin #-}

deleteMax :: forall x a. IntLikeMap x a -> IntLikeMap x a
deleteMax = coerce (IntMap.deleteMax @a)
{-# INLINE deleteMax #-}

deleteFindMin :: forall x a. (Coercible x Int) => IntLikeMap x a -> ((x, a), IntLikeMap x a)
deleteFindMin = coerce (IntMap.deleteFindMin @a)
{-# INLINE deleteFindMin #-}

deleteFindMax :: forall x a. (Coercible x Int) => IntLikeMap x a -> ((x, a), IntLikeMap x a)
deleteFindMax = coerce (IntMap.deleteFindMax @a)
{-# INLINE deleteFindMax #-}

updateMin :: forall x a. (a -> Maybe a) -> IntLikeMap x a -> IntLikeMap x a
updateMin = coerce (IntMap.updateMin @a)
{-# INLINE updateMin #-}

updateMax :: forall x a. (a -> Maybe a) -> IntLikeMap x a -> IntLikeMap x a
updateMax = coerce (IntMap.updateMax @a)
{-# INLINE updateMax #-}

updateMinWithKey :: forall x a. (Coercible x Int) => (x -> a -> Maybe a) -> IntLikeMap x a -> IntLikeMap x a
updateMinWithKey = coerce (IntMap.updateMinWithKey @a)
{-# INLINE updateMinWithKey #-}

updateMaxWithKey :: forall x a. (Coercible x Int) => (x -> a -> Maybe a) -> IntLikeMap x a -> IntLikeMap x a
updateMaxWithKey = coerce (IntMap.updateMaxWithKey @a)
{-# INLINE updateMaxWithKey #-}

minView :: forall x a. IntLikeMap x a -> Maybe (a, IntLikeMap x a)
minView = coerce (IntMap.minView @a)
{-# INLINE minView #-}

maxView :: forall x a. IntLikeMap x a -> Maybe (a, IntLikeMap x a)
maxView = coerce (IntMap.maxView @a)
{-# INLINE maxView #-}

minViewWithKey :: forall x a. (Coercible x Int) => IntLikeMap x a -> Maybe ((x, a), IntLikeMap x a)
minViewWithKey = coerce (IntMap.minViewWithKey @a)
{-# INLINE minViewWithKey #-}

maxViewWithKey :: forall x a. (Coercible x Int) => IntLikeMap x a -> Maybe ((x, a), IntLikeMap x a)
maxViewWithKey = coerce (IntMap.maxViewWithKey @a)
{-# INLINE maxViewWithKey #-}

-- Extras:

partialLookup :: forall x a. (Coercible x Int) => x -> IntLikeMap x a -> a
partialLookup x m = unIntLikeMap m IntMap.! coerce x
{-# INLINE partialLookup #-}

insertState :: (Coercible x Int) => (Maybe a -> b) -> x -> a -> IntLikeMap x a -> (b, IntLikeMap x a)
insertState f x a = coerce . IntMap.alterF (\m -> (f m, Just a)) (coerce x) . unIntLikeMap
{-# INLINE insertState #-}
