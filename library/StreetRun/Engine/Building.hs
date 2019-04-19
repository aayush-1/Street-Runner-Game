module StreetRunner.Engine.Building where

import Data.Text (Text)
import qualified Animate

data BuildingKey
  = BuildingKey'Idle
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName BuildingKey where
  keyName = BuildingKey'keyName

BuildingKey'keyName :: BuildingKey -> Text
BuildingKey'keyName = \case
  BuildingKey'Idle -> "Idle"
