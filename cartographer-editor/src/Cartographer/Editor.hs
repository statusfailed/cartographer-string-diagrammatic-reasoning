-- | A thin wrapper for Cartographer.Viewer.
--
-- This module adds buttons to connect ports and add generators to the diagram.
module Cartographer.Editor
  ( Model(..)
  , emptyModel
  , Action(..)
  , update
  , view
  , ActionState(..)
  , fromLayout
  ) where

import Cartographer.Editor.Types
import Cartographer.Editor.View
import Cartographer.Editor.Model
