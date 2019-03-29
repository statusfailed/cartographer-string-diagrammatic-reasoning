{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module defines how cartographer hypergraphs are rendered to SVG.
-- Here is a brief overview of the rendering approach.
--
-- The Cartographer.Layout keeps geometry information in "abstract"
-- coordinates- an integer grid, with no space for wires.
-- 
-- Cartographer.Draw is a simpler interface to Layout- namely, the Renderable
-- type can be used to draw all elements of the diagram individually.
--
-- This module makes two more transforms to get to the final screen
-- coordinates:
--  1) "make space" for wires, by interspersing an empty column between all
--      generator columns.
--  2) Scale up to pixel coordinates
module Cartographer.Viewer.View where

import Miso (View(..))
import qualified Miso as Miso
import Miso.String (ms)
import qualified Miso.String as MS
import qualified Miso.Svg as Svg

import Data.String

import Linear.Vector ((*^))
import Linear.V2 (V2(..))

import Cartographer.Layout (Layout, Tile(..))
import qualified Cartographer.Layout as Layout

import Cartographer.Types.Grid (Position)

import Cartographer.Draw (Renderable(..))
import qualified Cartographer.Draw as Draw

import Data.Equivalence (Equivalence)
import qualified Data.Equivalence as Equivalence

import Cartographer.Viewer.Drawing
import Cartographer.Viewer.Types

view :: Layout Generator -> ViewerOptions -> View RawAction
view layout opts = flip viewRenderable opts . Draw.toGridCoordinates $ layout

viewRenderable
  :: Draw.Renderable Generator Position -> ViewerOptions -> View RawAction
viewRenderable (Renderable tiles wires dimensions) opts =
  -- NOTE: order here is very important for clickability!
  -- If anything is above clickableGridSquares, then not all squares are
  -- clickable!
  Svg.svg_ svgAttrs
    [ diagramStyle
    , gridLines unitSize scaledDims
    , Svg.g_ [] (fmap g wires)
    , Svg.g_ [] (fmap f tiles)
    , clickableGridSquares spacedDims unitSize
    ]
  where
    unitSize = fromIntegral tileSize
    ViewerOptions tileSize = opts
    f t = uncurry viewTile t opts
    g t = uncurry viewWire t opts

  -- intersperse a "wires" col between every generator
  -- NOTE: the (- V2 1 0) removes the final unnecessary "wires" column from the
  -- grid.
    spacedDims = V2 2 1 * dimensions - V2 1 0
    scaledDims = fmap fromIntegral (tileSize *^ spacedDims)
    V2 imgWidth imgHeight = scaledDims
    svgAttrs      = [ Svg.height_ (ms imgHeight), Svg.width_ (ms imgWidth) ]

viewTile :: Tile Generator -> Position -> ViewerOptions -> View action
viewTile (TilePseudoNode p) v = viewPseudoNode p v
viewTile (TileHyperEdge  g) v = viewGenerator g v

-- | Render a wire in pixel coordinates between two integer grid positions
viewWire :: Position -> Position -> ViewerOptions -> View action
viewWire x y (ViewerOptions tileSize)
  = wrap $ connector (f 1 x) (f 0 y) -- dodgy af
  where
    scale   = (* V2 tileSize tileSize)
    stretch = (* V2 2 1)
    shift a = (+ V2 a 0)
    offset  = (+ V2 0 (fromIntegral tileSize / 2))
    f a = offset . fmap fromIntegral . scale . shift a . stretch

    wrap = Svg.g_ [Svg.class_' "wire"] . pure

-- | Draw a square grid spaced by unitSize pixels over the area specified by
-- the vector.
gridLines :: Double -> V2 Double -> View action
gridLines unitSize (V2 width height) =
  Svg.g_ [] [ Svg.g_ [] horizontal, Svg.g_ [] vertical ]

  where
    horizontal = fmap hline (enumFromThenTo 0 unitSize height)
    vertical   = fmap vline (enumFromThenTo 0 unitSize width)

    hline y = Svg.line_ ([ Svg.x1_ "0", Svg.x2_ (ms width)
                         , Svg.y1_ (ms y), Svg.y2_ (ms y) ] ++ displayOpts) []
    vline x = Svg.line_ ([ Svg.y1_ "0", Svg.y2_ (ms height)
                         , Svg.x1_ (ms x), Svg.x2_ (ms x) ] ++ displayOpts) []

    displayOpts = [ Svg.stroke_ "#eeeeee", Svg.strokeDasharray_ "5,5" ]

-- | Draw an invisible SVG rect, one for each grid square, so we can assign a
-- custom 'onClick' to each, and react when user clicks one.
clickableGridSquares :: V2 Int -> Double -> View RawAction
clickableGridSquares size@(V2 w h) unitSize =
  Svg.g_ []
    [ Svg.rect_
      [ Svg.width_ (ms unitSize), Svg.height_ (ms unitSize)
      , Svg.x_ (ms $ fromIntegral x * unitSize)
      , Svg.y_ (ms $ fromIntegral y * unitSize)
      , Svg.stroke_ "transparent"
      , Svg.strokeWidth_ "2"
      , Svg.fill_ "transparent"
      , Svg.onClick (RawClickedTile $ V2 x y)
      ] []
    | x <- [0..w]
    , y <- [0..h]
    ]

-- | View a pseudonode
-- TODO: make these movable! Will require use of the 'PseudoNode' ID.
viewPseudoNode :: Layout.PseudoNode -> V2 Int -> ViewerOptions -> View action
viewPseudoNode _ pos (ViewerOptions tileSize) = connectorWith "red" start end
  where
    unitSize = fromIntegral tileSize
    realPos = unitSize *^ V2 2 1 * fmap fromIntegral pos
    start   = realPos + V2 0.0 (unitSize / 2.0) :: V2 Double
    end     = start + V2 unitSize 0.0

-- View a Position-annotated generator
-- Generic drawing:
--    * black outline of total area
--    * vertically-centered circle (according to tileHeight)
--    * Symmetric bezier wires to circle from each port
viewGenerator
  :: Generator
  -> Position
  -> ViewerOptions
  -> View action
viewGenerator g@(Generator size ports color name) pos' (ViewerOptions tileSize)
  = 
  let pos = pos' * (V2 2 1)
      unitSize = fromIntegral tileSize
      height = unitSize * fromIntegral (tileHeight g) :: Double
      width  = unitSize
      v@(V2 x y) = fmap ((*unitSize) . fromIntegral) pos

      cx = x + width/2.0
      cy = y + height/2.0
      c = V2 cx cy

  in
    Svg.g_ [Svg.class_' "generator"]
      [ Svg.rect_
        [ Svg.width_ (ms width), Svg.height_ (ms height)
        , Svg.x_ (ms x), Svg.y_ (ms y)
        , Svg.stroke_ "transparent"
        , Svg.strokeWidth_ "2"
        , Svg.fill_ "transparent"
        ] []
      , Svg.g_ [] (fmap (viewGeneratorWire v c unitSize) lports) -- left ports
      , Svg.g_ [] (fmap (viewGeneratorWire v c unitSize) rports) -- right ports
      , Svg.circle_
        [ Svg.cx_ (ms cx), Svg.cy_ (ms cy), Svg.r_ (ms $ unitSize / 8)
        , Svg.stroke_ "black", Svg.strokeWidth_ "2", Svg.fill_ color ] []
      {-, clickTarget index unitSize v-}
      ]
  where
    lports = fmap Left (fst ports)
    rports = fmap Right (snd ports)


-- | View the wires connecting a generator's central shape to its ports
viewGeneratorWire
  :: V2 Double -- ^ Top-left coordinate
  -> V2 Double -- ^ Center coordinate
  -> Double    -- ^ Unit (tile) size
  -> Either Int Int -- ^ a port on the ith tile, either Left or Right side
  -> View action
viewGeneratorWire x cx unitSize port = connector cx (x + V2 px py)
  where
    px = either (const 0) (const unitSize) port
    py = (+ unitSize/2.0) . (*unitSize) . fromIntegral $ either id id port


-------------------------------
-- Constants

diagramStyle :: View action
diagramStyle = Svg.style_ [Svg.type_' "text/css"] [staticCss]

-- TODO
staticCss :: IsString s => s
staticCss =
  ".generator:hover > rect {\
  \  stroke: #dddddd;\
  \}\
  \ \
  \.clickTarget:hover {\
  \  fill: red;\
  \}"
