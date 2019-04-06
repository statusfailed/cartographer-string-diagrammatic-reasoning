{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A higher-order "collapsible" component
module Cartographer.Components.Collapsible where

import Miso

-- Model
data Model m = Model
  { _modelInnerModel :: m 
  , _modelState :: CollapsibleState
  } deriving(Eq, Ord, Show, Functor)

newModel :: m -> Model m
newModel m = Model m Expanded

data Action a = Toggle | InnerAction a
  deriving(Eq, Ord, Show, Functor)

data CollapsibleState = Expanded | Collapsed
  deriving(Eq, Ord, Show)

toggle :: CollapsibleState -> CollapsibleState
toggle Collapsed = Expanded
toggle Expanded = Collapsed

update :: (a -> m -> m) -> Action a -> Model m -> Model m
update _ Toggle (Model m s) = Model m (toggle s)
update updateInner (InnerAction a) (Model m s) = Model (updateInner a m) s

view :: (m -> View a) -> (m -> View a) -> Model m -> View (Action a)
view expanded collapsed (Model inner state) = Miso.div_ []
  [ Miso.div_ [] [ Miso.button_ [ onClick Toggle ] [ "show/hide" ] ]
  , Miso.div_ []
      [ case state of 
          Expanded  -> InnerAction <$> expanded inner
          Collapsed -> InnerAction <$> collapsed inner
      ]
  ]
