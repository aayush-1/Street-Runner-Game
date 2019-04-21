module StreetRunner.Config
  ( Config(..)
  , Resources(..)
  ) where

import qualified SDL

import StreetRunner.Resource

data Config = Config
  { cWindow :: SDL.Window
  , cRenderer :: SDL.Renderer
  , cResources :: Resources
  }
