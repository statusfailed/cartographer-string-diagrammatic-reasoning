{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The job of the 'Grid'  module is to manage the positions of the generators
-- in a Hypergraph.
-- It does not concern itself with whether nodes are pseudo or actual
-- generators, 
module Cartographer.Types.Grid where

import Linear.V2

import Cartographer.Types.Equivalence (Equivalence)
import qualified Cartographer.Types.Equivalence as Equivalence

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Position = V2 Int

newtype Height = Height { unHeight :: Int }
  deriving(Eq, Ord, Read, Show, Enum, Num)

-- | An integer grid containing a number of Nx1 tiles, arranged into layers
-- Each Position has at most one tile, but a tile occupies one or more
-- positions.
data Grid tile = Grid
  { _gridPositions :: Equivalence Position tile
  }
  deriving(Eq, Ord, Read, Show)

empty :: Grid tile
empty = Grid Equivalence.empty

-- | Insert columns, by simply shifting all tiles in columns >= the specified
-- column up by a specified amount.
shiftY
  :: Int -- ^ y coords >= this value will be shifted up by a specified amount.
  -> Int -- ^ Amount to increment by
  -> Grid tile
  -> Grid tile
shiftY i dy (Grid m) = Grid (Equivalence.mapElemsMonotonic f m)
  where f v@(V2 x y) = if y < i then v else V2 x (y+dy)

-- | Place a tile at a particular position.
-- If this causes overlap, other tiles in the same layer (y-coordinate) will be
-- shifted to make space.
placeTile :: Ord tile => tile -> Height -> Position -> Grid tile -> Grid tile
placeTile t h v (Grid g) = Grid (Equivalence.merge g updates)
  where
    -- All positions occupied by the tile
    vs    = [ v + V2 0 (unHeight y) | y <- [0..h] ]

    -- all the positions occupied by the new tile
    delta = Equivalence.fromList (fmap (,t) vs)

    updates = undefined -- TODO

-- Get the top-most position of each tile in the grid
-- (i.e., the position of its topmost tile).
positions :: Ord tile => Grid tile -> Map tile Position
positions g = undefined
