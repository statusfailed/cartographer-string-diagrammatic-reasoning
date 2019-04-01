{-# LANGUAGE OverloadedStrings #-}
module Cartographer.Editor.View where

import Miso (View(..))
import qualified Miso as Miso
import qualified Miso.Svg as Svg
import Miso.String (ms)

import qualified Data.Hypergraph as Hypergraph

import Cartographer.Layout (Layout(..))
import qualified Cartographer.Layout as Layout

import Cartographer.Editor.Types as Editor

import Cartographer.Viewer (Generator(..))
import qualified Cartographer.Viewer as Viewer

view :: [Generator] -> Model -> View Editor.Action
view gs (Model layout actionState) = Miso.div_ []
  [ toolbar gs
  , viewer layout vopts
  , infoFooter layout
  ]
  where vopts = Viewer.ViewerOptions 50 -- TODO: dont hard-code?

-- | Show the toolbar (above the viewer)
-- this consists of a "connect ports" button, and a number of generator
-- buttons, which can be placed into the current diagram.
toolbar :: [Generator] -> View Action
toolbar gs = Miso.div_ [] $ connectButton : fmap generatorButton gs

generatorButton :: Generator -> View Action
generatorButton g = Miso.button_ [Miso.onClick (StartPlaceGenerator g)]
  [ Svg.svg_ attrs [ Viewer.viewGenerator g 0 opts ] ]
  where 
    tileSize = 20
    opts = Viewer.ViewerOptions tileSize -- TODO: dont hardcode this?
    height = fromIntegral (Layout.generatorHeight g)
    attrs =
      [ Svg.height_ (ms $ height * tileSize)
      , Svg.width_ (ms tileSize)
      ]

-- | The "connect ports" button.
-- NOTE: using "free icons" from http://xahlee.info/comp/unicode_arrows.html
connectButton :: View Action
connectButton = Miso.button_ [Miso.onClick StartConnect] [ icon ]
  where
    icon = "↦"

-- | Show the viewer - the bottom pane. This just embeds the Viewer\'s action
-- type into the Editor\'s action type.
viewer :: Layout Generator -> Viewer.ViewerOptions -> View Action
viewer layout opts = ViewerAction <$> Viewer.view layout opts

-- | Show information about the hypergraph: its current dimensions, and whether
-- or not it's "valid" - i.e., fully connected up.
infoFooter :: Layout Generator -> View Action
infoFooter layout = Miso.div_ []
  [ Miso.h1_ [] [ Miso.text (ms . show $ Hypergraph.toSize hg)]
  , Miso.h1_ [] [ Miso.text (if Hypergraph.isComplete hg then "Valid" else "Invalid") ]
  ]
  where hg = hypergraph layout
