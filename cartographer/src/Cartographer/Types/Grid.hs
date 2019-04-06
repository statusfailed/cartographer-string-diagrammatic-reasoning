{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The job of the 'Grid'  module is to manage the positions of the generators
-- in a Hypergraph.
-- It does not concern itself with whether nodes are pseudo or actual
-- generators, merely how "tall" things are, what their positions are, and how
-- to avoid "collisions.
module Cartographer.Types.Grid where

-- todo: invariants:
--  1) if I place a tile of height n, I should see n squares occupied
--  2) if I place k tiles of height n, I should see k*n squares occupied
--  3) the squares occupied by the same tile should always be contiguous

import Prelude hiding (lookup)
import Linear.V2

import Data.Equivalence (Equivalence(..))
import qualified Data.Equivalence as Equivalence

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Foldable (minimum)
import Data.Maybe (isJust)

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

-- | returns the dimensions of the Grid in *tiles*
-- >>> dimensions empty == V2 0 0
-- >>> dimensions (placeTile () 1 (V2 0 0) empty) == V2 1 1
dimensions :: Grid tile -> V2 Int
dimensions g@(Grid (Equivalence cls _)) =
  case Cartographer.Types.Grid.null g of
    True  -> V2 0 0
    False -> V2 1 1 + maxCoords
  where
    maxCoords = foldl f (V2 0 0) . fmap fst $ Map.toList cls
    f (V2 mx my) (V2 x y) = V2 (max mx x) (max my y)

-- | Is the grid devoid of tiles?
null :: Grid tile -> Bool
null (Grid eq) = Equivalence.null eq

empty :: Grid tile
empty = Grid Equivalence.empty

-- | Shift tiles in a layer above a certain offset by a specified amount.
--
-- numEdges g == numEdges (shiftOffset v i g)
shiftOffset
  :: Ord tile
  => V2 Int
  -- ^ generators occupying positions with larger or equal y will be shifted
  -> Int
  -- ^ shift by this amount
  -> Grid tile
  -> Grid tile
shiftOffset v@(V2 x y) dy grid@(Grid eq) = Grid (Equivalence.mapElems f eq)
  where
    -- if a generator is at v, we have to shift from the TOP of that generator,
    -- not just the  middle, or the tiles of the generator will become
    -- discontinuous.
    V2 _ ystart = maybe v id $ flip positionOf grid . fst =<< lookup v grid

    -- NOTE: f must be injective (required by mapElems)
    f v'@(V2 x' y')
      | x == x' && y' >= ystart = V2 x' (y' + dy + (y - ystart))
      | otherwise               = v'

-- | Shift all tiles in or above a given layer up by a specified amount.
shiftLayer
  :: Ord tile
  => Int      -- ^ X coordinate
  -> Int      -- ^ Shift amount
  -> Grid tile
  -> Grid tile
shiftLayer x dx (Grid eq) = Grid (Equivalence.mapElems f eq)
  where
    f v@(V2 x' _)
      | x' >= x    = v + V2 dx 0
      | otherwise = v

removeLayer :: Ord tile => Int -> Grid tile -> Grid tile
removeLayer i (Grid eq) = Grid (Equivalence.mapElems f eq')
  where
    -- remove layer i
    eq' = Equivalence.filterElems (\(V2 x _) -> x /= i) eq

    f v@(V2 x' _)
      | x' > i    = v - V2 1 0
      | otherwise = v


-- | Place a tile at a particular position.
-- If this causes overlap, other tiles in the same layer (y-coordinate) will be
-- shifted to make space.
-- TODO: behaviour if Height < 1 means *nothing* added. Is that right?
placeTile :: Ord tile => tile -> Height -> Position -> Grid tile -> Grid tile
placeTile tile h v grid@(Grid eq) =
  -- check all occupied tiles that would conflict.
  case fmap fst $ filter (isJust . snd) contents of
    -- No conflicts; just drop in the tile.
    [] -> Grid (place eq)

    -- Conflicts- tile would overlap. First shift everything in the layer down
    -- by the required amount of space.
    -- TODO: shift by less- this shifts by height of inserted tile
    (conflict : _) ->
      let (Grid eq') = shiftOffset conflict (unHeight h) grid
      in  Grid (place eq')

  where
    -- All positions that will be occupied by the new tile.
    -- NOTE: `unHeight h - 1` *is* correct!
    vs = [ v + V2 0 y | y <- [0..unHeight h - 1] ]

    -- Contents of each of those positions
    contents = fmap (\x -> (x, Equivalence.classOf x eq)) vs

    -- Place the tile into the position equivalence.
    place g = foldr (flip Equivalence.equate tile) g vs

removeTile :: Ord tile => tile -> Grid tile -> Grid tile
removeTile t (Grid eq) = Grid (fst $ Equivalence.deleteClass t eq)

-- | Get the least-offset position of a tile in a 'Grid'.
-- In a left-to-right rendering, this means the topmost square of a tile.
positionOf :: Ord tile => tile -> Grid tile -> Maybe Position
positionOf tile grid =
  let s = Equivalence.membersOf tile (_gridPositions grid)
  in  if Set.null s then Nothing else Just . minimum $ Set.toList s

toList :: Ord tile => Grid tile -> [(tile, Set Position)]
toList (Grid eq) = Equivalence.toClasses eq

-- | Return the tile for a given position, along with the set of tiles it
-- occupies.
lookup :: Ord tile => Position -> Grid tile -> Maybe (tile, Set Position)
lookup v (Grid eq) = do
  c  <- Equivalence.classOf v eq
  let vs = Equivalence.membersOf c eq
  return (c, vs)

-- | An ascending list of all occupied tiles in the grid.
occupiedTiles :: Grid tile -> [(Position, tile)]
occupiedTiles = Map.toAscList . Equivalence._equivalenceClass . _gridPositions

-- Get the top-most position of each tile in the grid
-- (i.e., the position of its topmost tile).
--
-- TODO: this might produce the entire Map - might be better to provide as a
-- function to look up a specific tile?
--
-- NOTE: this is technically unsafe; if the set is empty, then it will crash
-- because minimum is partial.
-- However, the Equivalence should ensure that there are no empty classes in
-- the Equivalence.
positions :: Ord tile => Grid tile -> Map tile Position
positions = fmap minimum . _equivalenceMembers . _gridPositions
