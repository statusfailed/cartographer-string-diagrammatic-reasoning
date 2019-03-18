{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables #-}
module View where

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

import Drawing
import Types

data ViewOptions = ViewOptions
  { tileSize :: Int
  } deriving(Eq, Ord, Read, Show)

view :: Layout Generator -> ViewOptions -> View action
view layout opts = flip viewRenderable opts . Draw.toGridCoordinates $ layout

viewRenderable
  :: Draw.Renderable Generator Position -> ViewOptions -> View action
viewRenderable (Renderable tiles wires dimensions) opts@(ViewOptions tileSize) =
  Svg.svg_ svgAttrs
    [ diagramStyle
    , gridLines (fromIntegral tileSize) scaledDims
    , Svg.g_ [] (fmap f tiles)
    , Svg.g_ [] (fmap g wires)
    ]
  where
    f t = uncurry viewTile t opts
    g t = uncurry viewWire t opts

    -- TODO: remove random +5 to y dimension (fix Layout.dimensions!)
    scaledDims = fmap fromIntegral (tileSize *^ V2 2 1 * (V2 0 5 + dimensions))
    V2 imgWidth imgHeight = scaledDims
    svgAttrs      = [ Svg.height_ (ms imgHeight), Svg.width_ (ms imgWidth) ]

viewTile :: Tile Generator -> Position -> ViewOptions -> View action
viewTile (TilePseudoNode p) v = viewPseudoNode p v
viewTile (TileHyperEdge  g) v = viewGenerator g v

-- | Render a wire in pixel coordinates between two integer grid positions
viewWire :: Position -> Position -> ViewOptions -> View action
viewWire x y (ViewOptions tileSize)
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

-- | View a pseudonode
-- TODO: make these movable! Will require use of the 'PseudoNode' ID.
viewPseudoNode :: Layout.PseudoNode -> V2 Int -> ViewOptions -> View action
viewPseudoNode _ pos (ViewOptions tileSize) = connectorWith "red" start end
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
  -> ViewOptions
  -> View action
viewGenerator g@(Generator size ports color name) pos' (ViewOptions tileSize) =
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
