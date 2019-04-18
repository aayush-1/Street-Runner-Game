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
-- dsa = move duck jump hurt 
stepRunnerState :: Step RunnerAction -> RunnerState -> RunnerState --dstate (action height hurt recover)
stepRunnerState stepDa ds = case stepDa of
    Step'Change _ da -> case da of
      RunnerAction'Jump -> RunnerState da (Just 0) hurt recover
      RunnerAction'Hurt -> RunnerState da height (Just 0) (Just 0)
      _ -> RunnerState nextAction height hurt recover
    Step'Sustain _ -> RunnerState nextAction height hurt recover
  where
    nextAction
      | hurt /= Nothing = RunnerAction'Hurt
      | height /= Nothing = RunnerAction'Jump
      | otherwise = smash stepDa
    height = case dsHeight ds of
      Just p -> if p < 1 then Just (clamp (p + 0.04) 0 1) else Nothing -- rate at which jump is made (following sin wave) 
      Nothing -> Nothing
    hurt = case dsHurt ds of
      Just p -> if p < 3 then Just (clamp (p + 0.04) 2 3) else Nothing -- time for which it'll stay hurt
      Nothing -> Nothing
    recover = case dsRecover ds of
      Just p -> if p < 1 then Just (clamp (p + 0.01) 0 1) else Nothing -- recovery rate
      Nothing -> Nothing

stepRunnerPosition :: Step RunnerAction -> Animations RunnerKey -> Animate.Position RunnerKey Seconds -> Animate.Position RunnerKey Seconds
stepRunnerPosition (Step'Sustain _) animations pos = Animate.stepPosition animations pos frameDeltaSeconds
stepRunnerPosition (Step'Change _ da) _ _ = case da of
  RunnerAction'Move -> Animate.initPosition RunnerKey'Move
  RunnerAction'Duck -> Animate.initPosition RunnerKey'Sneak
  RunnerAction'Jump -> Animate.initPositionLoops RunnerKey'Kick 0
  RunnerAction'Hurt -> Animate.initPosition RunnerKey'Hurt

stepSpeed :: Step RunnerAction -> Percent -> Percent
stepSpeed sda speed = clamp speed' 1 25 --range of speed
  where
    da = smash sda
    speed' = case da of
      RunnerAction'Duck -> speed - 0.1 -- speed'll decrease with 0.1 
      RunnerAction'Move -> speed + 0.03 
      RunnerAction'Jump -> speed -- same
      RunnerAction'Hurt -> speed - 0.15

showRunner :: RunnerState -> Bool
showRunner RunnerState{dsRecover} = case dsRecover of
  Nothing -> True
  Just percent -> sin (1000 * (percent ** 3)) >= 0

addStocks :: Score -> Score -> Bool -- adding extra lives after some selected scores 
addStocks s s' = or [pass 1, pass 5, pass 10, pass 20, pass 35, pass 50, pass 75, powerOf50]
  where
    pass n = s < n && s' >= n
    powerOf50 = s' >= 100 && mod s 50 > mod s' 50

nextStocks :: Score -> Score -> Stocks -> Stocks --max lives one can gather at a run is 8
nextStocks s s' st = min 8 (st + if addStocks s s' then 1 else 0)

stepZoom :: Float -> RunnerAction -> Float --zoom and zoomout speed control 
stepZoom zoom RunnerAction = case RunnerAction of
  RunnerAction'Duck -> clamp (zoom - 0.05) 0 1 --range with rate of change
  _ -> clamp (zoom + 0.05) 0 1

applyHurt :: Bool -> Step RunnerAction -> Maybe Percent -> Step RunnerAction
applyHurt collision stepDa recover
  | collision && recover == Nothing = case stepDa of
      Step'Sustain RunnerAction'Hurt -> stepDa
      Step'Sustain da -> Step'Change da RunnerAction'Hurt
      Step'Change da _ -> Step'Change da RunnerAction'Hurt
  | otherwise = stepDa
