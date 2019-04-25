module StreetRun.Scene
  ( SceneManager(..)
  , Scene(..)
  ) where

import StreetRun.Scene_change

class Monad m => SceneManager m where
  toScene :: Scene -> m ()

