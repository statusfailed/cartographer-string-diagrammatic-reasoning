{-# LANGUAGE OverloadedStrings #-}
module Cartographer.Viewer.Drawing where

import Miso(View(..))
import qualified Miso.Svg as Svg
import Miso.String (MisoString)
import Data.String

import Linear.Vector ((*^))
import Linear.V2 (V2(..))

import Cartographer.Types.Grid (Position)

-------------------------------
-- Drawing connections

drawConnector :: Position -> Position -> Double -> View action
drawConnector start end unitSize
  = connector (scale start unitSize) (scale end 0)
  where scale v a = (unitSize *^ V2 2 1 * fmap fromIntegral v) + V2 a (unitSize/2)

-------------------------------
-- Bezier utilities

showPathV2 :: (IsString s, Show a) => V2 a -> s
showPathV2 (V2 x y) = fromString $ mconcat [ show x, " ", show y ]

bezierControls :: Fractional a => V2 a -> V2 a -> (V2 a, V2 a)
bezierControls x y = (x + dx, y - dx)
  where
    dx = V2 (getX (y - x) / 2) 0
    getX (V2 x _) = x

bezierPath :: (Fractional a, Show a, IsString s, Monoid s) => V2 a -> V2 a -> s
bezierPath a b = mconcat $
  [ "M ", showPathV2 a
  , " C ", showPathV2 c1, " "
  , showPathV2 c2, " ", showPathV2 b
  ]
  where
    (c1, c2) = bezierControls a b

connectorWith
  :: (Fractional a, Show a) => MisoString -> V2 a -> V2 a -> View action
connectorWith color a b = Svg.path_
  [ Svg.d_ (bezierPath a b)
  , Svg.stroke_ color
  , Svg.fill_ "none"
  ] []

connector
  :: (Fractional a, Show a) => V2 a -> V2 a -> View action
connector = connectorWith "black"
