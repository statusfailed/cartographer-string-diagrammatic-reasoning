{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Data.Hypergraph.Type
  ( Signature(..)
  , HyperEdgeId(..)
  , Port(..)
  , ClosedHypergraph(..)
  , OpenHypergraph(..)
  , Source
  , Target
  , PortRole(..)
  , Open(..)
  , Wire(..)
  , open
  , portRole
  , toPortRole
  , isBoundary
  , toHyperEdgeId
  , Hypergraph(..)
  , empty
  , identity
  , twist
  , addEdge
  , deleteEdge
  , connect
  , isConnectedTo
  , toWire
  , source
  , target
  , isComplete
  , prettyPrint
  , prettyWires
  ) where

import Data.Foldable
import Control.Applicative (liftA2)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Data.Equivalence (Equivalence)
import qualified Data.Equivalence as Equivalence

import Data.Functor.Identity (Identity(..))

-- Used to turn a type-level Source/Target into a value level one.
import Data.Reflection
import Data.Proxy

class Signature a where
  toSize :: a -> (Int, Int)

instance Integral a => Signature (a, a) where
  toSize (x, y) = (fromIntegral x, fromIntegral y)

-- | Uniquely identify each edge of a hypergraph.
-- NOTE: Does not say what the "shape" of each generator is.
newtype HyperEdgeId = HyperEdgeId { unHyperEdgeId :: Int }
  deriving(Eq, Ord, Read, Show, Enum, Num)

-- phantom types for type safety
data Source
data Target

-- | A Reified version of the Source and Target types.
-- We use this in Layout to determine if a Boundary is on the left or right of
-- the diagram without having to repeat code for each type.
data PortRole = Source | Target
  deriving(Eq, Ord, Read, Show)

instance Reifies Source PortRole where
  reflect _ = Source

instance Reifies Target PortRole where
  reflect _ = Target

-- | Ports of a hyperedge.
-- This is parametrised by f to allow ports to specify boundary ports as well
-- as generator ports.
data Port a f = Port (f HyperEdgeId) Int

deriving instance Eq   (f HyperEdgeId) => Eq (Port a f)
deriving instance Ord  (f HyperEdgeId) => Ord (Port a f)
deriving instance Read (f HyperEdgeId) => Read (Port a f)
deriving instance Show (f HyperEdgeId) => Show (Port a f)

-- | Convert a 'Port a f' to a Prox
toProxy :: Port a f -> Proxy a
toProxy _ = Proxy

toPortRole :: Reifies a PortRole => Port a f -> PortRole
toPortRole = reflect . toProxy

-- | scott-encoded version- avoid having to case split everywhere.
portRole :: Reifies a PortRole => b -> b -> Port a f -> b
portRole s t p = case toPortRole p of
  Source -> s
  Target -> t

-- | Is an 'Open' 'Port' a Boundary?
isBoundary :: Port a Open -> Bool
isBoundary (Port e _) = open True (const False) e

toHyperEdgeId :: Port a Open -> Maybe HyperEdgeId
toHyperEdgeId (Port Boundary _) = Nothing
toHyperEdgeId (Port (Gen e) _)  = Just e

-- | The type of Hypergraphs, parametrised by the type of generators (sig).
-- By using different types for "f" we can make this open or closed
-- hypergraphs.
--
-- NOTE: we explicitly ignore the "nodes" of the hypergraph in this type.
-- That's because of the monogamicity requirement of the paper: no node can
-- appear in more than two hyperedges, and must be a boundary node if it
-- appears in only one.
-- Another way to view this is that nodes are still present, but are identified
-- uniquely by the two Ports they connect. In an 'OpenHypergraph', this
-- corresponds to having the "Source" port on the Boundary.
data Hypergraph f sig = Hypergraph
  { connections     :: Bimap (Port Source f) (Port Target f)
  , signatures      :: Map HyperEdgeId sig
  , nextHyperEdgeId :: HyperEdgeId
  }

type Wire f = (Port Source f, Port Target f)

-- | NOTE: using UndecidableInstances here, but we don't really need it because
-- we never actually need to parametrise by the "f" type.
--
-- However, I\'m keeping the ClosedHypergraph type (and therefore the need for
-- UndecidableInstances) because I think it clarifies how the OpenHypergraph
-- type works.
deriving instance (Eq sig  , Eq   (f HyperEdgeId)) => Eq (Hypergraph f sig)
deriving instance (Ord sig , Ord  (f HyperEdgeId)) => Ord (Hypergraph f sig)
-- odd, why do we need an Ord instance here?
-- deriving instance (Read sig, Ord  (f HyperEdgeId), Read (f HyperEdgeId)) => Read (Hypergraph f sig)
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
data Open a = Boundary | Gen a
  deriving(Eq, Ord, Read, Show)

-- The obvious functor definition
instance Functor Open where
  fmap f (Gen x)  = Gen (f x)
  fmap f Boundary = Boundary

-- This is the same Applicative instance that 'Maybe' has, but it doesnt make a
-- whole lot of sense to me, even though it seems to be OK...
instance Applicative Open where
  pure = Gen
  liftA2 f (Gen a) (Gen b) = Gen (f a b)
  liftA2 f _ _ = Boundary

-- | Fold over an 'Open' value.
open :: b -> (a -> b) -> Open a -> b
open b _ Boundary = b
open _ f (Gen x)  = f x

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

-- Open hypergraphs have a signature - the number of input and output ports
-- connected to on the boundary.
instance Signature sig => Signature (OpenHypergraph sig) where
  toSize = maxBoundaryPorts

-------------------------------
-- Basic graphs

-- | The empty hypergraph
empty :: Hypergraph Open sig
empty = Hypergraph Bimap.empty Map.empty 0

-- | The identity morphism
identity :: Hypergraph Open sig
identity = Hypergraph conns sigs 0
  where
    conns = Bimap.fromList [(Port Boundary 0, Port Boundary 0)]
    sigs  = Map.empty

-- | the "twist" morphism
twist = Hypergraph conns Map.empty 0 where
  conns = Bimap.fromList
    [ (Port Boundary 0, Port Boundary 1)
    , (Port Boundary 1, Port Boundary 0)
    ]

-- | The number of inputs and outputs of a hypergraph.
-- This is taken as the maximum connected boundary port, or zero if none are
-- connected.
maxBoundaryPorts :: Hypergraph Open sig -> (Int, Int)
maxBoundaryPorts hg = (n, k)
  where
    ws = (Bimap.toList . connections) hg
    ns = ws >>= f . fst
    ks = ws >>= f . snd

    -- + 1 because ports are zero-indexed.
    n = case ns of [] -> 0; xs -> maximum xs + 1
    k = case ks of [] -> 0; xs -> maximum xs + 1

    f :: Port a Open -> [Int]
    f (Port Boundary i) = [i]
    f _ = []

-- | Add an edge to a 'Hypergraph'.
-- TODO: don't let user of this module assign the hyperedge ID-
-- they could easily break the graph (and replace an existing generator, which
-- might end up with fewer ports, and then weird invalid connections)
addEdge :: sig -> Hypergraph f sig -> (HyperEdgeId, Hypergraph f sig)
addEdge sig g = (edgeId, g
  { connections     = connections g -- new edge is unconnected
  , signatures      = Map.insert edgeId sig (signatures g)
  , nextHyperEdgeId = succ edgeId 
  })
  where
    edgeId = nextHyperEdgeId g

-- | Delete an edge from a 'Hypergraph'
deleteEdge
  :: (Applicative f, Eq (f HyperEdgeId), Ord (f HyperEdgeId))
  => HyperEdgeId -> Hypergraph f sig -> Hypergraph f sig
deleteEdge e g = g
  { connections = removeWiresOf e (connections g)
  , signatures  = Map.filterWithKey (\k _ -> k /= e) (signatures g)
  }
  where
    removeWiresOf e =
      Bimap.filter (\s t -> not $ s `isPortOf` e || t `isPortOf` e)

-- | 'p `isPortOf` e' returns True if p is a port on the generator e
isPortOf
  :: (Eq (f HyperEdgeId), Applicative f)
  => Port a f -> HyperEdgeId -> Bool
isPortOf (Port fe _) e = pure e == fe

-- | Connect two ports in the hypergraph.
-- If the source or target port was already connected to something, that
-- connection is overwritten.
--
-- As-is, this does not prevent cycles.
-- CARTOGRAPHER relies on the Layout class to enforce this, by only allowing
-- ports in shallower layers to connect to deeper ones.
connect
  :: (Eq (f HyperEdgeId), Ord (f HyperEdgeId))
  => Port Source f
  -- ^ source port
  -> Port Target f
  -- ^ target port
  -> Hypergraph f sig
  -- ^ Hypergraph to modify
  -> Hypergraph f sig
-- overwrites connection if p1 or p2 was already connected!
connect p1 p2 hg = hg { connections = Bimap.insert p1 p2 (connections hg) }

-- | Is a source connected to a particular target in the hypergraph?
isConnectedTo
  :: (Eq (f HyperEdgeId), Ord (f HyperEdgeId))
  => Port Source f -> Port Target f -> Hypergraph f sig -> Bool
isConnectedTo s t = maybe False (==t) . Bimap.lookup s . connections

-- | Given a source or target port, get the Wire it belongs to.
toWire
  :: (Eq (f HyperEdgeId), Ord (f HyperEdgeId), Reifies a PortRole)
  => Port a f
  -> Hypergraph f sig
  -> Maybe (Port Source f, Port Target f)
toWire p@(Port e i) hg = case toPortRole p of
  Source ->
    let p' = Port e i in (p',) <$> Bimap.lookup  p' (connections hg)
  Target ->
    let p' = Port e i in (,p') <$> Bimap.lookupR p' (connections hg)

-- | What is the /source/ port that connects to this /target/, if any?
source
  :: (Eq (f HyperEdgeId), Ord (f HyperEdgeId))
  => Port Target f -> Hypergraph f sig -> Maybe (Port Source f)
source t = fmap fst . toWire t

-- | What is the /target/ port that this /source/ connects to, if any?
target
  :: (Eq (f HyperEdgeId), Ord (f HyperEdgeId))
  => Port Source f -> Hypergraph f sig -> Maybe (Port Target f)
target s = fmap snd . toWire s

-- | Are all the ports of this hypergraph connected to something?
-- Yes if 2 * numWires == numPorts
isComplete :: Signature sig => Hypergraph Open sig -> Bool
isComplete hg = numPorts == 2 * numWires
  where
    numWires = length (Bimap.toList $ connections hg)
    numPorts = foldl (\s (i,o) -> s + i + o) 0 allPorts
    allPorts = toSize hg : fmap toSize (toList $ signatures hg)

-------------------------------
-- Pretty printing
-- example:
--
--  signature
--  ---------------
--  0 == <show sig>
--  1 == <show sig>
--
--  wires
--  ---------------
--  0 port 0  --> 1 port 2

prettyPrint :: Show sig => OpenHypergraph sig -> String
prettyPrint (Hypergraph conns signatures _)
  =  "signatures\n"
  ++ "-------------------------------\n"
  ++  prettySignature (Map.toList signatures)
  ++ "\n"
  ++ "wires\n"
  ++ "-------------------------------\n"
  ++ prettyWires (Bimap.toList conns)
  ++ "\n"

prettySignature :: Show a => [(HyperEdgeId, a)] -> String
prettySignature xs =
  xs >>= (\(HyperEdgeId i, sig) -> show i ++ " == " ++ show sig ++ "\n")

prettyWires :: [(Port Source Open, Port Target Open)] -> String
prettyWires xs =
  xs >>= (\(s,t) -> prettyPort s ++ "  -->  " ++ prettyPort t ++ "\n")

prettyPort (Port (Gen (HyperEdgeId e)) i) = show e ++ " port " ++ show i
prettyPort (Port Boundary i) = "B port " ++ show i
