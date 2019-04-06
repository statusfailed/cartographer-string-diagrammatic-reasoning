{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
module Cartographer.Components.RuleBuilder where

import Miso

import Cartographer.Layout (Layout(..))
import qualified Cartographer.Proof as Proof
import Cartographer.Viewer.Types (Generator(..))

import qualified Cartographer.Layout as Layout
import qualified Cartographer.Editor as Editor

type Model = (Editor.Model, Editor.Model)
emptyModel = (Editor.emptyModel, Editor.emptyModel)

-- Left and Right are useful constructor names :)
type Action = Either Editor.Action Editor.Action

update :: Action -> Model -> Model
update (Left  a) (l, r) = (Editor.update a l, r)
update (Right a) (l, r) = (l, Editor.update a r)

-- TODO: add diagnostics about whether left and right are:
--    * complete
--    * have same dimensions
view :: [Generator] -> Model -> View Action
view gs (l, r) = Miso.div_ [Miso.style_ [("display", "flex")]]
  [ Miso.div_ [ Miso.style_ [("flex", "50%")] ] [ Left  <$> Editor.view gs l ]
  , Miso.div_ [ Miso.style_ [("flex", "50%")] ] [ Right <$> Editor.view gs r ]
  ]

toRule :: Model -> Maybe (Proof.Rule Generator)
toRule (Editor.Model l _, Editor.Model r _) = Proof.rule l r
