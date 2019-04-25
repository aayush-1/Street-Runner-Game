module StreetRun.Scene
  ( SceneManager(..)
  , Scene(..)
  ) where

import StreetRun.Scenario

class Monad m => SceneManager m where
  toScene :: Scene -> m ()

