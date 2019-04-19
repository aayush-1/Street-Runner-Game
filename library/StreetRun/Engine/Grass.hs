module StreetRunner.Engine.Grass where

import Data.Text (Text)
import qualified Animate

data GrassKey
  = GrassKey'Idle
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName GrassKey where
  keyName = mountainKey'keyName

mountainKey'keyName :: GrassKey -> Text
mountainKey'keyName = \case
  GrassKey'Idle -> "Idle"
