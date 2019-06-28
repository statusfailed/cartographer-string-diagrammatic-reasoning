module Data.Hypergraph.Rewriting where

import Data.Maybe (fromJust)
import Data.List (foldl')
import Control.Arrow ((***))

import Data.Hypergraph.Type
import Data.Hypergraph.Matching (Matching(..))

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | Delete, leaving a "gap" in the hypergraph.
-- This must be filled, or the hypegraph is not valid.
unsafeDelete :: Matching a -> OpenHypergraph a -> OpenHypergraph a
unsafeDelete (Matching wires edges) g = g
  { connections = foldl' (flip Bimap.delete) (connections g) deadWires
  , signatures  = foldl' (flip Map.delete)   (signatures g)  deadEdges
  }
  where
    -- snd: RHS is context graph
    -- fst: LHS is "source' wire. could use snd and deleteR too.
    deadWires = fst . snd <$> Bimap.toList wires
    deadEdges = snd <$> Bimap.toList edges

-- | 'unsafeSplice matching context value' fills "value" into the context graph
-- at the boundary positions specified in the supplied matching.
unsafeSplice
  :: Matching a -> OpenHypergraph a -> OpenHypergraph a -> OpenHypergraph a
unsafeSplice matching replacement context = context
  { signatures =
      foldl' (flip $ uncurry Map.insert) (signatures context) newEdges
  , connections =
      foldl' (flip $ uncurry Bimap.insert) (connections context) newWires
  , nextHyperEdgeId = maxRep + maxCtx
  }
  where
    maxRep = nextHyperEdgeId replacement
    maxCtx = nextHyperEdgeId context

    newEdges = (\(e,s) -> (e+maxCtx, s)) <$> Map.toList (signatures replacement)
    newWires = fixWire <$> Bimap.toList (connections replacement)

    toSource :: (Wire Open, Wire Open) -> (Port Source Open, Port Source Open)
    toSource ((s,_), (s', _)) = (s, s')

    toTarget :: (Wire Open, Wire Open) -> (Port Target Open, Port Target Open)
    toTarget ((_,t), (_,t')) = (t, t')

    wires = Bimap.toList $ _matchingWires matching

    sources = Bimap.fromList . fmap toSource $ wires
    targets = Bimap.fromList . fmap toTarget $ wires

    -- TODO: no more fromJust, boo!
    fixWire :: Wire Open -> Wire Open
    fixWire (s, t) = fromJust $ do
      s' <- fixPort sources s
      t' <- fixPort targets t
      return (s', t')

    fixPort m p@(Port Boundary i) = Bimap.lookup p m
    fixPort _ (Port (Gen e) i) = pure $ Port (Gen (e + maxCtx)) i

-- NOTE: this will fail if the matching is not a matching of the supplied
-- pattern. Just expose as a Maybe value?
rewrite
  :: Matching a       -- ^ matching representing a "hole" in the graph
  -> OpenHypergraph a -- ^ replacement for the "hole"
  -> OpenHypergraph a -- ^ context in which to rewrite
  -> OpenHypergraph a
rewrite matching pattern =
  unsafeSplice matching pattern . unsafeDelete matching
