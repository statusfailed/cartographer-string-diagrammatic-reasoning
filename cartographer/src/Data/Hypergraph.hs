{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Hypergraph
  ( Signature(..)
  , HyperEdgeId(..)
  , Port(..)
  , ClosedHypergraph(..)
  , OpenHypergraph(..)
  , Open(..)
  , Hypergraph(..)
  , empty
  , identity
  , addEdge
  , connect
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Functor.Identity (Identity(..))

class Signature a where
  toSize :: a -> (Int, Int)

instance Integral a => Signature (a, a) where
  toSize (x, y) = (fromIntegral x, fromIntegral y)

-- | Uniquely identify each edge of a hypergraph.
-- NOTE: Does not say what the "shape" of each generator is.
newtype HyperEdgeId = HyperEdgeId { unHyperEdgeId :: Int }
  deriving(Eq, Ord, Read, Show, Enum, Num)

data Port f = Port (f HyperEdgeId) Int

deriving instance Eq   (f HyperEdgeId) => Eq (Port f)
deriving instance Ord  (f HyperEdgeId) => Ord (Port f)
deriving instance Read (f HyperEdgeId) => Read (Port f)
deriving instance Show (f HyperEdgeId) => Show (Port f)

-- | The type of Hypergraphs, parametrised by the type of generators (sig).
-- By using different types for "f" we can make this open or closed
-- hypergraphs.
--
-- NOTE: we explicitly ignore the "nodes" of the hypergraph in this type.
-- That's because of the monogamicity requirement of the paper: no node can
-- appear in more than two hyperedges, and must be a boundary node if it
-- appears in only one.
data Hypergraph f sig = Hypergraph
  { connections :: Map (Port f) (Port f)
  , signatures  :: Map HyperEdgeId sig
  }

-- | NOTE: using UndecidableInstances here, but we don't really need it because
-- we never actually need to parametrise by the "f" type.
--
-- However, I\'m keeping the ClosedHypergraph type (and therefore the need for
-- UndecidableInstances) because I think it clarifies how the OpenHypergraph
-- type works.
deriving instance (Eq sig  , Eq   (f HyperEdgeId)) => Eq (Hypergraph f sig)
deriving instance (Ord sig , Ord  (f HyperEdgeId)) => Ord (Hypergraph f sig)
-- odd, why do we need an Ord instance here?
deriving instance (Read sig, Ord  (f HyperEdgeId), Read (f HyperEdgeId)) => Read (Hypergraph f sig)
deriving instance (Show sig, Show (f HyperEdgeId)) => Show (Hypergraph f sig)

-- | The type of closed Hypergraphs, i.e. those hypergraphs with no "dangling
-- wires".
-- We don't use it, but just provide it in contrast to the 'OpenHypergraph'
-- type.
type ClosedHypergraph sig = Hypergraph Identity sig

-- | The type of "generators" in an open hypergraph.
-- This type essentially extends the set of hyperedges with left boundary
-- and a right boundary of arbitrary size.
--
-- NOTE: when a is a number, this type essentially the extended reals
-- Could use ExtendedReal but NegInf + PosInf is undefined (error).
data Open a = OpenLeft | Gen a | OpenRight
  deriving(Eq, Ord, Read, Show)

-- | The type of open hypergraphs.
-- Instead of allowing "dangling wires", we explicitly have 0xN and Mx0
-- generators for the left and right boundaries for an open hypergraph of type
-- (N, M).
-- NOTE: the "signatures" map won't contain the "Left" and "Right generators,
-- because they don't really have a "signature" - their size depends purely
-- on what is connected to them.
-- We use the convention that the largest port number i (i.e. in a connection
-- Port Left i) is the size of the boundary.
type OpenHypergraph sig = Hypergraph Open sig

-------------------------------
-- Can we build useful graphs?

-- | The empty hypergraph
empty :: Hypergraph Open sig
empty = Hypergraph Map.empty Map.empty

-- | The identity morphism
identity :: Hypergraph Open sig
identity = Hypergraph conns sigs
  where
    conns = Map.fromList [(Port OpenLeft 0, Port OpenRight 0)]
    sigs  = Map.empty

-- | the "twist" morphism
twist = Hypergraph conns Map.empty where
  conns = Map.fromList
    [ (Port OpenLeft 0, Port OpenRight 1)
    , (Port OpenLeft 1, Port OpenRight 0)
    ]

-- | Add an edge to a 'Hypergraph'.
-- TODO: don't let user of this module assign the hyperedge ID-
-- they could easily break the graph (and replace an existing generator, which
-- might end up with fewer ports, and then weird invalid connections)
addEdge
  :: HyperEdgeId -> sig -> Hypergraph f sig -> Hypergraph f sig
addEdge e sig g = g
  { connections = connections g -- new edge is unconnected
  , signatures  = Map.insert e sig (signatures g)
  }

-- | Connect two ports in the hypergraph.
-- If the source port was already connected to something, that connection is
-- overwritten.
connect
  :: (Eq (f HyperEdgeId), Ord (f HyperEdgeId))
  => Port f
  -- ^ source port
  -> Port f
  -- ^ target port
  -> Hypergraph f sig
  -- ^ Hypergraph to modify
  -> Hypergraph f sig
-- overwrites connection if p1 or p2 was already connected!
connect p1 p2 hg = hg { connections = Map.insert p1 p2 (connections hg) }
