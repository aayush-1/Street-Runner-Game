module StreetRun.Runner where

import qualified Safe
import qualified Animate
import Data.Text (Text)
import Linear (V2(..))
import KeyState

import StreetRun.Camera
import StreetRun.CFL
import StreetRun.Types
import StreetRun.Hurdle
import StreetRun.Hurdle_1
import StreetRun.Input
import StreetRun.Step
import StreetRun.Physics

data RunnerAction
  = RunnerAction'Move
  | RunnerAction'Duck
  | RunnerAction'Jump
  | RunnerAction'Hurt
  deriving (Show, Eq)

data RunnerState = RunnerState
  { dsAction :: RunnerAction
  , dsHeight :: Maybe Percent
  , dsHurt :: Maybe Percent
  , dsRecover :: Maybe Percent
  } deriving (Show, Eq)

data RunnerKey
  = RunnerKey'Idle
  | RunnerKey'Move
  | RunnerKey'Kick
  | RunnerKey'Hurt
  | RunnerKey'Sneak
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Animate.KeyName RunnerKey where  --JSON parsing 
  keyName = runnerKey'keyName

runnerKey'keyName :: RunnerKey -> Text   -- same number of cases in json file   
runnerKey'keyName = \case
  RunnerKey'Idle -> "Idle"
  RunnerKey'Move -> "Move"
  RunnerKey'Kick -> "Kick"
  RunnerKey'Hurt -> "Hurt"
  RunnerKey'Sneak -> "Sneak"


