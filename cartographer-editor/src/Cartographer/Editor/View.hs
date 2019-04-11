{-# LANGUAGE OverloadedStrings #-}
module Cartographer.Editor.View where

import Miso
import qualified Miso as Miso
import qualified Miso.Svg as Svg
import Miso.String (ms)
import Data.String

import qualified Data.Hypergraph as Hypergraph

import Cartographer.Layout (Layout(..))
import qualified Cartographer.Layout as Layout

import Cartographer.Editor.Types as Editor

import Cartographer.Viewer (Generator(..))
import qualified Cartographer.Viewer as Viewer

view :: [Generator] -> Model -> View Editor.Action
view gs (Model layout actionState highlights) = Miso.div_ [ class_ "box" ]
  [ sizeMsg (Hypergraph.toSize $ hypergraph layout)
  , generatorBar gs
  , viewer highlights layout Viewer.defaultOptions
  , toolbar
  , infoFooter layout
  ]

-- TODO: add is-focused class if the button is in its corresponding
-- "ActionState"
toolbar :: View Action
toolbar = div_ [class_ "buttons"]
  [ button_ [ class_ "button is-primary", onClick StartDeleteGenerator ]
    [ "delete generator" ]
--  , button_ [ class_ "button is-primary", onClick StartMoveGenerator ]
--    [ "move" ]
  , button_ [ class_ "button is-primary", onClick ClearDiagram ]
    [ "clear diagram" ]
  , button_ [ class_ "button is-primary", onClick StartDisconnect ]
    [ "disconnect wires" ]
  ]

-- | Show the generator bar - a toolbar appearing above the viewer
--
-- this consists of a number of generator buttons, which can be placed into the
-- current diagram.
generatorBar :: [Generator] -> View Action
generatorBar gs = Miso.div_ [] (fmap generatorButton gs)

generatorButton :: Generator -> View Action
generatorButton g =
  Miso.button_ [class_ "button", Miso.onClick (StartPlaceGenerator g)]
    [ Svg.svg_ attrs [ Viewer.viewGenerator g 0 opts ] ]
  where
    -- TODO: dont hardcode?
    opts   = Viewer.defaultOptions { Viewer.tileSize = 20 }
    height = fromIntegral (Layout.generatorHeight g)
    attrs =
      [ Svg.height_ (ms $ height * Viewer.tileSize opts)
      , Svg.width_ (ms $ Viewer.tileSize opts)
      ]

-- | Show the viewer - the bottom pane. This just embeds the Viewer\'s action
-- type into the Editor\'s action type.
viewer
  :: Hypergraph.MatchState Generator
  -> Layout Generator
  -> Viewer.ViewerOptions
  -> View Action
viewer m layout opts = ViewerAction <$> Viewer.viewWith m layout opts

-- | Show information about the hypergraph: its current dimensions, and whether
-- or not it's "valid" - i.e., fully connected up.
infoFooter :: Layout Generator -> View Action
infoFooter layout = Miso.div_ []
  [ if Hypergraph.isComplete hg then div_ [] [] else warningMsg ]
  where
    hg = hypergraph layout
    warningMsg =
      div_ [ class_ "notification is-warning" ]
        [ "diagram contains unconnected ports" ]

sizeMsg (k,n) =
  div_ [ class_ "notification is-info" ]
    [ fromString $ "type: " ++ show k ++ " → " ++ show n ]
