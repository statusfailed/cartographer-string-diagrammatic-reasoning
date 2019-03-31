module Cartographer.Viewer.Model
  ( toAction
  ) where

import Control.Monad.Reader.Class

import Cartographer.Layout (Layout)
import qualified Cartographer.Layout as Layout

import Cartographer.Viewer.Types

type Model = Layout Generator
