module StreetRun.Scenario where 

import Data.Text (Text)
import qualified Animate

data Scene
  = Scene'Title
  | Scene'Play
  | Scene'Pause
  | Scene'Death
  | Scene'GameOver
  | Scene'Quit
  deriving (Show, Eq)

data GrassKey
  = GrassKey'Idle
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName GrassKey where
  keyName = grassKey'keyName

grassKey'keyName :: GrassKey -> Text
grassKey'keyName = \case
  GrassKey'Idle -> "Idle"

data BuildingKey
  = BuildingKey'Idle
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName BuildingKey where
  keyName = buildingKey'keyName

buildingKey'keyName :: BuildingKey -> Text
buildingKey'keyName = \case
  BuildingKey'Idle -> "Idle"
