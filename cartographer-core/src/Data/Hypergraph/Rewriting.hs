module Data.Hypergraph.Rewriting where

import Data.Maybe (fromJust, isJust)
import Data.List (foldl')
import Control.Arrow ((***))
import Control.Monad

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
--
-- If the matching is not valid, Nothing is returned.
splice
  :: Matching a       -- ^ A set of wires and edges
  -> OpenHypergraph a -- ^ A replacement for the matching
  -> OpenHypergraph a -- ^ the context of the matching
  -> Maybe (OpenHypergraph a)
splice matching replacement context = do
  newWires <- mapM fixWire $ Bimap.toList (connections replacement)
  -- verify that each boundary in the matching has a corresponding port in the
  -- replacement.
  -- TODO: this is kinda nasty, maybe there's a better way?
  guard $ all (hasRhsWire . fst) (Bimap.toAscList $ _matchingWires matching)
  return $ context
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

    toSource :: (Wire Open, Wire Open) -> (Port Source Open, Port Source Open)
    toSource ((s,_), (s', _)) = (s, s')

    toTarget :: (Wire Open, Wire Open) -> (Port Target Open, Port Target Open)
    toTarget ((_,t), (_,t')) = (t, t')

    wires = Bimap.toList $ _matchingWires matching

    sources = Bimap.fromList . fmap toSource $ wires
    targets = Bimap.fromList . fmap toTarget $ wires

    hasRhsWire (s, t)
      =  (not (isBoundary s) || f (Bimap.lookup  s (connections replacement)))
      && (not (isBoundary t) || f (Bimap.lookupR t (connections replacement)))
      where
        f = isJust

    -- TODO: no more fromJust, boo!
    fixWire :: Wire Open -> Maybe (Wire Open)
    fixWire (s, t) = do
      s' <- fixPort sources s
      t' <- fixPort targets t
      return (s', t')

    fixPort m p@(Port Boundary i) = Bimap.lookup p m
    fixPort _ (Port (Gen e) i) = pure $ Port (Gen (e + maxCtx)) i

-- NOTE: this will fail if the matching is not a matching of the supplied
-- pattern. Just expose as a Maybe value?
rewrite'
  :: Matching a       -- ^ matching representing a "hole" in the graph
  -> OpenHypergraph a -- ^ replacement for the "hole"
  -> OpenHypergraph a -- ^ context in which to rewrite
  -> Maybe (OpenHypergraph a)
rewrite' matching pattern =
  splice matching pattern . unsafeDelete matching

-- | Given a matching of some pattern, and a replacement for that pattern,
-- delete the pattern from the context, and replace with the replacement.
--
-- If the matching is not of the same size as the replacement, the original
-- context is returned, without rewriting.
rewrite
  :: Matching a -> OpenHypergraph a -> OpenHypergraph a -> OpenHypergraph a
rewrite m p c = maybe c id $ rewrite' m p c
