module StreetRun.Config
  ( Config(..)
  , Resources(..)
  ) where

import qualified SDL

import StreetRun.Assets
import StreetRun.Assets_1
import StreetRun.Assets_2


data Config = Config
  { cWindow :: SDL.Window
  , cRenderer :: SDL.Renderer
  , cResources :: Resources
  }
