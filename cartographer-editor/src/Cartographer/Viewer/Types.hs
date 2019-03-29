module Cartographer.Viewer.Types where

import Miso.String (MisoString)
import Data.Hypergraph
import qualified Cartographer.Layout as Layout
import Cartographer.Types.Grid (Position)
import Linear.V2 (V2(..))

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
  generatorHeight  = tileHeight
  generatorInputs  = fst . ports
  generatorOutputs = snd . ports

-- | Height (in tiles) of the generator.
-- TODO: explain this better.
-- A generator of size (2,1) will have total height 3, so it looks symmetric.
tileHeight (Generator (l, r) _ _ _) = max l r + modifier
  where modifier = if l == 0 || r == 0 then 0 else mod (l+r) 2

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
--    An empty grid square
data Action
  = ClickTile ClickedTile
  deriving(Eq, Ord, Read, Show)

data ClickedTile = ClickedTile
  { _clickedTilePosition :: V2 Int
  -- ^ The position of the tile that was clicked, in screen-space integer
  -- coordinates (i.e., including wires)
  , _clickedTileEntity :: Either ClickedWires ClickedGenerator
  -- ^ The entity at the clicked tile.
  } deriving(Eq, Ord, Read, Show)

-- | The user clicked a "wires", column between two generator columns.
-- The argument is the 
data ClickedWires = ClickedWires (V2 Int)
  deriving(Eq, Ord, Read, Show)

-- | A 'ClickedGenerator' represents the contents of a tile that was clicked,
-- and was occupied by a generator.
data ClickedGenerator = ClickedGenerator
  { _clickedGeneratorHyperEdgeId :: HyperEdgeId
  , _clickedGeneratorGridCoords  :: Position -- ^ position in "abstract coords"
  -- ^ The generator clicked on
  , _clickedGeneratorInputPort   :: Maybe (Port Target Open)
  -- ^ The input  port on this tile, if any.
  , _clickedGeneratorOutputPort  :: Maybe (Port Source Open)
  -- ^ The output port on this tile, if any.
  } deriving(Eq, Ord, Read, Show)
