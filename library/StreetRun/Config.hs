module StreetRun.Config
  ( Config(..)
  , Resources(..)
  ) where

import qualified SDL

import StreetRun.Resource

data Config = Config
  { cWindow :: SDL.Window
  , cRenderer :: SDL.Renderer
  , cResources :: Resources
  }
