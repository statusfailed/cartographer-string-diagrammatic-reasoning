{-# LANGUAGE TupleSections #-}
module Data.Hypergraph.Index where

import Data.Hypergraph.Type
import Data.Maybe (catMaybes)
import Data.Function
import Data.List

import Control.Monad

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

-------------------------------
-- generic index

type Index k t = Map k [t]

-- | Create an 'Index' from a list.
fromList :: (Foldable t, Ord k) => t (k, a) -> Map k [a]
fromList = update Map.empty

-- | update an index with a list of new values and their keys
update :: (Foldable t, Ord k) => Map k [a] -> t (k, a) -> Map k [a]
update = foldl' (\m (k, v) -> Map.alter (f v) k m)
  where
    f v Nothing   = Just [v]
    f v (Just vs) = Just (v:vs)

lookup :: Ord k => k -> Index k t -> Maybe [t]
lookup = Map.lookup

-------------------------------
-- for matching

type HypergraphIndex a = Index ((a, Int), (a, Int)) (Wire Open)

-- | /O(n log n)/ for an 'OpenHypergraph' with /n/ connections.
fromHypergraph :: Ord a => OpenHypergraph a -> HypergraphIndex a
fromHypergraph g
  = fromList . catMaybes . fmap (toKey g) . Bimap.toList
  $ connections g

toKey
  :: Ord a
  => OpenHypergraph a -> Wire Open -> Maybe (((a, Int), (a, Int)), Wire Open)
toKey g w@(s, t) = do
  s' <- toKeyPort g s
  t' <- toKeyPort g t
  return ((s', t'), w)

toKeyPort :: OpenHypergraph sig -> Port a Open -> Maybe (sig, Int)
toKeyPort g (Port Boundary i) = Nothing
toKeyPort g (Port (Gen e)  i) = (,i) <$> Map.lookup e (signatures g)
