module StreetRunner.Engine.Boy where

import Data.Text (Text)
import qualified Animate

data BoyKey
  = BoyKey'Idle
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName BoyKey where
  keyName = boyKey'keyName

boyKey'keyName :: BoyKey -> Text
boyKey'keyName = \case
  BoyKey'Idle -> "Idle"