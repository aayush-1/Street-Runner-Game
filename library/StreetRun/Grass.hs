module StreetRun.Grass where

import Data.Text (Text)
import qualified Animate

data GrassKey
  = GrassKey'Idle
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName GrassKey where
  keyName = grassKey'keyName

grassKey'keyName :: GrassKey -> Text
grassKey'keyName = \case
  GrassKey'Idle -> "Idle"
