{-# LANGUAGE DeriveFunctor #-}
module Cartographer.Components.ProofBuilder where

import Miso

data Model = Model
  deriving(Eq, Ord, Show)

emptyModel :: Model
emptyModel = Model

data Action = Action
  deriving(Eq, Ord, Show)

update :: Action -> Model -> Model
update Action Model = Model

view :: Model -> View Action
view Model = Miso.div_ [] []
