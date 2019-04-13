module StreetRun.Engine.Mountain where

import Data.Text (Text)
import qualified Animate

data MountainKey
  = MountainKey'Idle
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName MountainKey where
  keyName = mountainKey'keyName

mountainKey'keyName :: MountainKey -> Text
mountainKey'keyName = \case  --lambda case expression : Lamdacase is included in cabal file 
  MountainKey'Idle -> "Idle"
