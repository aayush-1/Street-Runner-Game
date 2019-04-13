module StreetRun.Engine.River where

import Data.Text (Text)
import qualified Animate

data RiverKey
  = RiverKey'Idle
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName RiverKey where --Animate.Keyname used for JSON parsing ::key->Text 
  keyName = riverKey'keyName

riverKey'keyName :: RiverKey -> Text
riverKey'keyName = \case
  RiverKey'Idle -> "Idle"
