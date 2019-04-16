module StreetRun.Engine.Runner where

import qualified Safe
import qualified Animate
import Data.Text (Text)
import Linear (V2(..))
import KeyState

import StreetRun.Engine.Camera
import StreetRun.Engine.Frame
import StreetRun.Engine.Types
import StreetRun.Engine.Obstacle
import StreetRun.Engine.Input
import StreetRun.Engine.Step
import StreetRun.Engine.Physics

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
  keyName = RunnerKey'keyName

RunnerKey'keyName :: RunnerKey -> Text -- same number of cases in json file 
RunnerKey'keyName = \case
  RunnerKey'Idle -> "Idle"
  RunnerKey'Move -> "Move"
  RunnerKey'Kick -> "Kick"
  RunnerKey'Hurt -> "Hurt"
  RunnerKey'Sneak -> "Sneak"

RunnerX :: Float
RunnerX = 250 -- X position of the runner

RunnerY :: Num a => a  -- Y position from top 
RunnerY = 16 * 26 - 8 

duckCamera :: Camera -- RunnerY is added to make it focus in region near Runner
duckCamera = Camera (V2 ((screenWidth) / 2) ((screenHeight + RunnerY) / 2)) (V2 2 2) -- (Point to focus on)(stretch ratio of x and y axis)

rightEdge :: Float
rightEdge = arenaWidth - (RunnerX + 48)

RunnerHeight :: Maybe Percent -> Int
RunnerHeight p = truncate (RunnerHeight' p)

RunnerHeight' :: Maybe Percent -> Float
RunnerHeight' (Just (Percent percent)) = sin (percent * pi) * (-32 * 7) + RunnerY
RunnerHeight' _ = RunnerY

RunnerAabb :: Maybe Percent -> Aabb
RunnerAabb maybeHeight = Aabb (V2 (RunnerX + 4) y) (V2 (RunnerX + 24) (y + 48))
  where
    y = RunnerHeight' maybeHeight

distanceFromLastObstacle :: [(Float, ObstacleTag)] -> Float
distanceFromLastObstacle obstacles = case Safe.lastMay obstacles of
  Nothing -> rightEdge
  Just (dist, _) -> rightEdge - dist

stepRunnerAction :: Input -> RunnerState -> Step RunnerAction
stepRunnerAction input ds = case da of
  RunnerAction'Move -> case ksStatus (iUp input) of --ksStatus(Keystatus as const) to check the status of that key 
    KeyStatus'Pressed -> Step'Change da RunnerAction'Jump
    KeyStatus'Held -> Step'Change da RunnerAction'Jump
    _ -> case ksStatus (iDown input) of --if after up, down cases are checked
      KeyStatus'Pressed -> Step'Change da RunnerAction'Duck -- changed to duck
      KeyStatus'Held -> Step'Change da RunnerAction'Duck -- duck sustained 
      _ -> Step'Sustain RunnerAction'Move -- unaffected by any other key
  RunnerAction'Duck -> case ksStatus (iUp input) of
    KeyStatus'Pressed -> Step'Change da RunnerAction'Jump
    KeyStatus'Held -> Step'Change da RunnerAction'Jump
    _ -> case ksStatus (iDown input) of
      KeyStatus'Pressed -> Step'Sustain RunnerAction'Duck
      KeyStatus'Held -> Step'Sustain RunnerAction'Duck
      _ -> Step'Change da RunnerAction'Move
  RunnerAction'Jump -> case dsHeight ds of
    Nothing -> Step'Change da RunnerAction'Move
    Just p -> if p < 1 then Step'Sustain da else Step'Change da RunnerAction'Move
  RunnerAction'Hurt -> case dsHurt ds of
    Nothing -> Step'Change da RunnerAction'Move
    Just p -> if p < 1 then Step'Sustain da else Step'Change da RunnerAction'Move
  where
    da = dsAction ds