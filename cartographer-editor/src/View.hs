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

import Cartographer.Layout (Layout)
import qualified Cartographer.Layout as Layout

import Cartographer.Types.Grid (Position)

import Data.Equivalence (Equivalence)
import qualified Data.Equivalence as Equivalence

import Drawing
import Types

data ViewOptions = ViewOptions
  { tileSize :: Int
  } deriving(Eq, Ord, Read, Show)

viewLayout :: Layout Generator -> ViewOptions -> View action
viewLayout layout opts@(ViewOptions tileSize) =
  Svg.svg_ svgAttrs
    $ style
    : gridLines unitSize (fromIntegral <$> dims)
    : (renderedGenerators ++ renderedConnectors)
  where
    unitSize = fromIntegral tileSize
    sz = V2 tileSize tileSize * V2 2 1 -- double width to accomodate connectors
    dims@(V2 imgWidth imgHeight) = sz * (Layout.dimensions layout + V2 1 1)
    svgAttrs = [ Svg.height_ (ms imgHeight), Svg.width_ (ms imgWidth) ]
    style = Svg.style_ [Svg.type_' "text/css"] [staticCss]

    renderedGenerators = fmap (f viewGenerator) (Layout.positioned layout)
    f g (x,y) = g x y opts

    renderedConnectors
      = fmap (($unitSize) . uncurry drawConnector) (Layout.connectors layout)



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

-- View a Position-annotated generator
-- Generic drawing:
--    * black outline of total area
--    * vertically-centered circle (according to tileHeight)
--    * Symmetric bezier wires to circle from each port
viewGenerator
  :: Generator
  -> V2 Int
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
