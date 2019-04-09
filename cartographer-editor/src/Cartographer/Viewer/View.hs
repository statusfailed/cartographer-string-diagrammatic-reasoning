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

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Data.Hypergraph (MatchState(..), emptyMatchState, Wire(..), Open(..))
import Cartographer.Layout (Layout, Tile(..))
import qualified Cartographer.Layout as Layout

import Cartographer.Types.Grid (Position)

import Cartographer.Draw (Renderable(..))
import qualified Cartographer.Draw as Draw

import Data.Equivalence (Equivalence)
import qualified Data.Equivalence as Equivalence

import Cartographer.Viewer.Drawing
import Cartographer.Viewer.Types

import Cartographer.Viewer.Model (toAction)

viewWith
  :: MatchState Generator -> Layout Generator -> ViewerOptions -> View Action
viewWith m layout opts = flip toAction layout <$> viewRawWith m layout opts

view :: Layout Generator -> ViewerOptions -> View Action
view = viewWith emptyMatchState

viewRawWith
  :: MatchState Generator
  -> Layout Generator
  -> ViewerOptions
  -> View RawAction
viewRawWith m layout opts
  = flip (viewRenderableWith m) opts . Draw.toGridCoordinates $ layout

viewRaw :: Layout Generator -> ViewerOptions -> View RawAction
viewRaw = viewRawWith emptyMatchState

viewRenderable
  :: Draw.Renderable Generator Position -> ViewerOptions -> View RawAction
viewRenderable = viewRenderableWith emptyMatchState

viewRenderableWith
  :: MatchState Generator
  -> Draw.Renderable Generator Position
  -> ViewerOptions
  -> View RawAction
viewRenderableWith m (Renderable tiles wires dimensions) opts =
  -- NOTE: order here is very important for clickability!
  -- If anything is above clickableGridSquares, then not all squares are
  -- clickable!
  Svg.svg_ svgAttrs
    [ diagramStyle
    , gridLines unitSize (scaledDims + V2 0 unitSize)
    , Svg.g_ [] (fmap g wires)
    , Svg.g_ [] (fmap f tiles)
    , clickableGridSquares spacedDims unitSize
    ]
  where
    unitSize = fromIntegral tileSize
    ViewerOptions tileSize highlightColor = opts
    f t     = uncurry (viewTile m) t opts
    g (w,(v1,v2)) = viewWire m w v1 v2 opts

  -- intersperse a "wires" col between every generator
  -- NOTE: the (- V2 1 0) removes the final unnecessary "wires" column from the
  -- grid.
    spacedDims = V2 2 1 * dimensions - V2 1 0
    scaledDims = fmap fromIntegral (tileSize *^ spacedDims)
    V2 imgWidth imgHeight = scaledDims + V2 0 unitSize
    svgAttrs      = [ Svg.height_ (ms imgHeight), Svg.width_ (ms imgWidth) ]

viewTile
  :: MatchState Generator
  -> Tile Generator
  -> Position
  -> ViewerOptions
  -> View action
viewTile m (TilePseudoNode p) v = viewPseudoNode m p v
viewTile m (TileHyperEdge  g) v = viewGenerator g v

-- | Render a wire in pixel coordinates between two integer grid positions
viewWire
  :: MatchState Generator
  -> Wire Open
  -> Position
  -> Position
  -> ViewerOptions
  -> View action
viewWire m (s,t) x y (ViewerOptions tileSize highlightColor)
  = wrap $ connectorWith color (f 1 x) (f 0 y) -- dodgy af
  where
    scale   = (* V2 tileSize tileSize)
    stretch = (* V2 2 1)
    shift a = (+ V2 a 0)
    offset  = (+ V2 0 (fromIntegral tileSize / 2))
    f a = offset . fmap fromIntegral . scale . shift a . stretch

    wrap = Svg.g_ [Svg.class_' "wire"] . pure

    sourceHighlight = Bimap.memberR s (_matchStatePortsSource m)
    targetHighlight = Bimap.memberR t (_matchStatePortsTarget m)
    color = case sourceHighlight && targetHighlight of
      True  -> highlightColor
      False -> "black"

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

    displayOpts = [ Svg.stroke_ "#cccccc", Svg.strokeDasharray_ "5,5" ]

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
viewPseudoNode
  :: MatchState Generator
  -> Layout.PseudoNode
  -> V2 Int
  -> ViewerOptions
  -> View action
viewPseudoNode m pn pos (ViewerOptions tileSize highlightColor) =
  connectorWith color start end
  where
    unitSize = fromIntegral tileSize
    realPos = unitSize *^ V2 2 1 * fmap fromIntegral pos
    start   = realPos + V2 0.0 (unitSize / 2.0) :: V2 Double
    end     = start + V2 unitSize 0.0

    -- TODO: this appears twice, factor into function?
    (Layout.PseudoNode s t _) = pn
    sourceHighlight = Bimap.memberR s (_matchStatePortsSource m)
    targetHighlight = Bimap.memberR t (_matchStatePortsTarget m)
    color = case sourceHighlight && targetHighlight of
      True  -> highlightColor
      False -> "black"

-- View a Position-annotated generator
-- Generic drawing:
--    * black outline of total area
--    * vertically-centered circle (according to generatorHeight)
--    * Symmetric bezier wires to circle from each port
viewGenerator
  :: Generator
  -> Position
  -> ViewerOptions
  -> View action
viewGenerator g@(Generator size ports color name) pos' opts
  = 
  let
      ViewerOptions tileSize _ = opts
      pos = pos' * (V2 2 1)
      unitSize = fromIntegral tileSize
      height = unitSize * fromIntegral (Layout.generatorHeight g) :: Double
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
