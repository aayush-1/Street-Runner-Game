module StreetRun.Manager.Scene
  ( SceneManager(..)
  , Scene(..)
  ) where

import StreetRun.Engine.Scene

class Monad m => SceneManager m where
  toScene :: Scene -> m ()

