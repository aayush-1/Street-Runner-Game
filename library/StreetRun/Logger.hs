module StreetRun.Logger where

import Data.Text (Text)

class Monad m => Logger m where
  logText :: Text -> m ()