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
view gs (l, r) = div_ [ Miso.class_ "message is-info" ]
  [ div_ [ class_ "message-header" ]
    [ "Rule"
    , button_ [ class_ "delete" ] [] -- TODO: hook this up
    ]
  , div_ [ class_ "message-body" ]
    [ div_ [ class_ "columns" ]
      [ col . box $ Left  <$> Editor.view gs l
      , col . box $ Right <$> Editor.view gs r
      ]
    ]
  ]
  where
    col = div_ [ class_ "column" ] . pure
    box = div_ [ class_ "box" ] . pure

toRule :: Model -> Maybe (Proof.Rule Generator)
toRule (Editor.Model l _, Editor.Model r _) = Proof.rule l r
