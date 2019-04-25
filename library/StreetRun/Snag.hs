module StreetRun.Snag where

import Data.Text (Text)
import qualified Animate

data BirdKey
  = BirdKey'Idle
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName BirdKey where
  keyName = birdKey'keyName

birdKey'keyName :: BirdKey -> Text
birdKey'keyName = \case
  BirdKey'Idle -> "Idle"



data LavaKey
  = LavaKey'Idle
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName LavaKey where
  keyName = lavaKey'keyName

lavaKey'keyName :: LavaKey -> Text
lavaKey'keyName = \case
  LavaKey'Idle -> "Idle"



data RockKey
  = RockKey'Idle
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName RockKey where
  keyName = rockKey'keyName

rockKey'keyName :: RockKey -> Text
rockKey'keyName = \case
  RockKey'Idle -> "Idle"
