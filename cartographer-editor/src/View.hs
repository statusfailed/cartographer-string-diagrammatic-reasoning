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
import Linear.V2 (V2(..))

import Cartographer.Layout (Layout)
import qualified Cartographer.Layout as Layout

import Cartographer.Types.Equivalence (Equivalence)
import qualified Cartographer.Types.Equivalence as Equivalence

data ViewOptions = ViewOptions
  { tileSize :: Int
  } deriving(Eq, Ord, Read, Show)

viewLayout :: Layout sig -> ViewOptions -> View action
viewLayout layout (ViewOptions tileSize) =
  Svg.svg_ svgAttrs $
    style : gridLines unitSize (fromIntegral <$> dims) : renderedGenerators
  where
    unitSize = fromIntegral tileSize
    sz = V2 tileSize tileSize
    dims@(V2 imgWidth imgHeight) = sz * (Layout.dimensions layout + V2 1 1)
    svgAttrs = [ Svg.height_ (ms imgHeight), Svg.width_ (ms imgWidth) ]
    style = Svg.style_ [Svg.type_' "text/css"] [staticCss]

    -- TODO
    renderedGenerators = []

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
