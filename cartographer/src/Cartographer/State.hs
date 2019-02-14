module Cartographer.State where

import Data.Hypergraph
import Linear.V2

type Position = V2 Int

-- | The top-level datatype
data CartographerState sig = CartographerState
  { hypergraph :: Hypergraph
  , positions  :: Map HyperEdgeId Position
  , portMap    :: Map (HyperEdgeId, Port) Vertex
  -- ^ map between ports and vertices
  } deriving(Eq, Ord, Read, Show)


-- TODO:
--    1) add to hypergraph
--    2) add to positions
--    3) move down any colloding nodes
addHyperEdge :: sig -> Position -> CartographerState sig -> CartographerState sig
addHyperEdge e s = undefined

connect
  :: (HyperEdgeId, Port)
  -> (HyperEdgeId, Port)
  -> CartographerState sig
  -> CartographerState sig
connect a b s = undefined

-- | Set a hyperedge's position.
-- 
-- TODO:
--    1) Verify that position is not left of an upstream edge
move :: HyperEdgeId -> Position -> CartographerState -> CartographerState
move e p s = undefined
