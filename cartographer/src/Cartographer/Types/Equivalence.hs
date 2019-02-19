module Cartographer.Types.Equivalence where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Equivalence a c = Equivalence
  { _equivalenceClass   :: Map a c
  -- ^ The classes of each member
  , _equivalenceMembers :: Map c (Set a)
  -- ^ The members of each class
  } deriving(Eq, Ord, Read, Show)

-- | An equivalence on the empty set
empty :: Equivalence a c
empty = Equivalence Map.empty Map.empty

-- | create an 'Equivalence' from a list of equivalences 'a ~ c'.
-- If a value 'a' appears more than once for a different 'c' (i.e., the list
-- does not represent a function), then the final value will be taken.
fromList :: (Ord c, Ord a) => [(a,c)] -> Equivalence a c
fromList = foldr (uncurry equate) empty

-- Merge two equivalences.
-- This is right-biased, so if an element 'a' appears in both the left and
-- right equivalences, the right equivalence's class will be taken.
merge :: Equivalence a c -> Equivalence a c -> Equivalence a c
merge eqa eqb = undefined

-- | Remove an element from the equivalence
-- If the element is not present, do nothing.
delete :: (Ord a, Ord c) => a -> Equivalence a c -> Equivalence a c
delete a eq@(Equivalence cls members) = case Map.lookup a cls of
  Nothing -> eq
  Just c -> Equivalence (Map.delete a cls) (Map.adjust (Set.delete a) c members)

-- | Put 'a' into the equivalence class 'c'
-- NOTE: to ensure that the Equivalence remains a partition,
-- if 'a' already appears under the key 'c', then it will first be removed.
equate :: (Ord a, Ord c) => a -> c -> Equivalence a c -> Equivalence a c
equate a c = equateNew a c . delete a

-- | Equate an element 'a' with the class 'c'.
-- 'a' must not already be part of the Equivalence
-- /This precondition is not checked./
equateNew :: (Ord a, Ord c) => a -> c -> Equivalence a c -> Equivalence a c
equateNew a c (Equivalence cls members) = Equivalence
  (Map.insert a c cls)
  (Map.alter (updateSet a) c members)
  where
    updateSet c = Just . maybe (Set.singleton c) (Set.insert c)

-- | Fetch the class of an element, if it has one.
classOf :: Ord a => a -> Equivalence a c -> Maybe c
classOf a = Map.lookup a . _equivalenceClass

-- | Return all the members of a class, if any.
membersOf :: Ord c => c -> Equivalence a c -> Set a
membersOf c = maybe Set.empty id . Map.lookup c . _equivalenceMembers

-- TODO
-- | Remove all elements matching a predicate from the Equivalence.
filterElems
  :: (Ord a, Ord c) => (a -> Bool) -> Equivalence a c -> Equivalence a c
filterElems f (Equivalence cls members) = Equivalence keep members'
  where
    (keep, discard) = Map.partitionWithKey (\a _ ->  f a) cls
    -- for each class, remove any "removed elements" from its set of elements.
    -- NOTE TODO: this is very inefficient; we don't need to fmap- there's no
    -- need to affect all the keys, just the ones that were deleted.
    deleted = Set.fromList (fmap fst . Map.toList $ discard)
    members' = fmap (flip Set.difference deleted) members

-- | NOTE: uses mapKeysMonotonic on the underlying maps, which means that
-- the supplied function must obey the condition that x < y => f x < f y.
-- /this condition is not checked/.
mapElemsMonotonic :: Ord a => (a -> b) -> Equivalence a c -> Equivalence b c
mapElemsMonotonic f (Equivalence cls members) = Equivalence
  (Map.mapKeysMonotonic f cls)
  undefined -- TODO: finish this function
