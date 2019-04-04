{-# LANGUAGE FlexibleContexts #-}
module Cartographer.Viewer.Types where

import Control.Monad.Reader.Class
import Linear.V2 (V2(..))

import Miso.String (MisoString)

import Data.Hypergraph

import Cartographer.Layout (Layout(..))
import qualified Cartographer.Layout as Layout
import Cartographer.Types.Grid (Position)

-- | Options to draw the Viewer
data ViewerOptions = ViewerOptions
  { tileSize :: Int
  } deriving(Eq, Ord, Read, Show)

-- | The viewer works with a concrete Generator type, which describes how
-- it can be drawn
data Generator = Generator
  { size :: (Int, Int)
  -- offsets of inputs and outputs. TODO: maybe throw this all away lol
  , ports :: ([Int], [Int])
  , color :: MisoString
  , name :: MisoString
  } deriving(Eq, Ord, Read, Show)

instance Signature Generator where
  toSize = size

instance Layout.Generator Generator where
  generatorHeight  = uncurry tileHeight . size
  generatorInputs  = fst . ports
  generatorOutputs = snd . ports

-- | Height (in tiles) of the generator.
-- This is picked as the smallest odd number greater than the max number of
-- inputs or outputs.
-- A generator of size (2,1) will have total height 3, so it looks symmetric.
tileHeight :: Int -> Int -> Int
tileHeight l r = let m = max l r in if even m then m + 1 else m

-------------------------------
-- Actions that can happen in the Viewer

-- | User clicked a tile. Coordinates in "expanded tile space"
-- This is intended to be immediately converted to a ClickEntity
data RawAction = RawClickedTile (V2 Int)
  deriving(Eq, Ord, Read, Show)

-- | An action in the viewer is only to click something in the diagram.
-- This can be any of, for example:
--    A port on a boundary generator
--    A tile belonging to a generator, but without a port
--    An empty grid square in a "wires" column
--    An empty grid square in a "generators" column
data Action = Action
  { _actionPosition :: V2 Int
  -- ^ The position of the tile that was clicked, in spaced integer coords
  -- (i.e., including wires)
  , _actionClickedPorts    :: Maybe ClickedPorts
  -- ^ The displayed grid is "wires" columns interspersed between "generator"
  -- columns. If a "wires" column is clicked, this is Nothing.
  } deriving(Eq, Ord, Read, Show)

-- | A 'ClickedPorts' represents the (up to two) ports that were present
-- at a particular grid coordinate.
-- It also contains the boundary grid coordinates of the click.
data ClickedPorts = ClickedPorts
  { _clickedPortsGridCoords  :: Position
  -- ^ position in "abstract coords"
  , _clickedPortsInputPort   :: Maybe (Port Target Open)
  -- ^ The input  port on this tile, if any.
  , _clickedPortsOutputPort  :: Maybe (Port Source Open)
  -- ^ The output port on this tile, if any.
  } deriving(Eq, Ord, Read, Show)

-------------------------------
-- Convert from a RawAction to an Action

-- | Given a 'RawAction' - a click on the grid at particular coordinates -
-- translate this into an 'Action', which tells us what was at those
-- coordinates.
toAction
  :: (Layout.Generator sig, MonadReader (Layout sig) m)
  => RawAction
  -> m Action
toAction (RawClickedTile v@(V2 x y)) = reader $ \layout -> case even x of
  -- x is even: we're in a "generator" column
  True  -> Action v (Just $ clickedPorts v layout)

  -- x is odd: we're in a "wires" column- we just pass the position unchanged.
  False -> Action v Nothing

-- | NOTE: this is kind of a crappy implementation because x has to be odd,
-- which is super weird?
clickedPorts
  :: (Layout.Generator sig, MonadReader (Layout sig) m)
  => V2 Int
  -> m ClickedPorts
clickedPorts (V2 x y) = reader $ \layout ->
  uncurry (ClickedPorts v') $ Layout.lookup v' layout
  where
    v' = V2 (x `div` 2) y
