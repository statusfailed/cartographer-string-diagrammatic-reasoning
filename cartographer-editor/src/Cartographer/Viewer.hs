-- | The cartographer Viewer is an "interactive primitive" for working with a
-- 'Cartographer.Layout'. 
--
-- You should import this module qualified.
--
-- If you want editor actions like connecting ports, you should use
-- 'Cartographer.Editor', which wraps this module.
module Cartographer.Viewer
  ( Generator(..)
  , Action(..)
  , view
  , ViewerOptions(..)
  ) where

import Cartographer.Viewer.Types
import Cartographer.Viewer.View
