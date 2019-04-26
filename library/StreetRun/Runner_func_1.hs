module StreetRun.Runner_func_1 where


import qualified Safe
import qualified Animate
import Data.Text (Text)
import Linear (V2(..))
import KeyState
import StreetRun.Runner_func_2
import StreetRun.Runner
import StreetRun.Camera
import StreetRun.Frame
import StreetRun.Types
import StreetRun.Hurdle
import StreetRun.Hurdle_1
import StreetRun.Input
import StreetRun.Step
import StreetRun.Physics

stepRunnerAction :: Input -> RunnerState -> Step RunnerAction
stepRunnerAction input ds = case da of
  RunnerAction'Move -> case ksStatus (iUp input) of    --ksStatus(Keystatus as const) to check the status of that key 
    KeyStatus'Pressed -> Step'Change da RunnerAction'Jump
    KeyStatus'Held -> Step'Change da RunnerAction'Jump
    _ -> case ksStatus (iDown input) of   --if after up, down cases are checked
      KeyStatus'Pressed -> Step'Change da RunnerAction'Duck
      KeyStatus'Held -> Step'Change da RunnerAction'Duck
      _ -> Step'Sustain RunnerAction'Move
  -- RunnerAction'Duck -> case ksStatus (iUp input) of
  --   KeyStatus'Pressed -> Step'Change da RunnerAction'Jump
  --   KeyStatus'Held -> Step'Change da RunnerAction'Jump
  --   _ -> case ksStatus (iDown input) of
  --     KeyStatus'Pressed -> Step'Sustain RunnerAction'Duck
  --     KeyStatus'Held -> Step'Sustain RunnerAction'Duck
  --     _ -> Step'Change da RunnerAction'Move
  RunnerAction'Duck -> case dsHeight ds of
    Nothing -> Step'Change da RunnerAction'Move
    Just p -> if p < 1 then Step'Sustain da else Step'Change da RunnerAction'Move  
  RunnerAction'Jump -> case dsHeight ds of
    Nothing -> Step'Change da RunnerAction'Move
    Just p -> if p < 1 then Step'Sustain da else Step'Change da RunnerAction'Move
  RunnerAction'Hurt -> case dsHurt ds of
    Nothing -> Step'Change da RunnerAction'Move
    Just p -> if p < 1 then Step'Sustain da else Step'Change da RunnerAction'Move
  where
    da = dsAction ds

stepRunnerState :: Step RunnerAction -> RunnerState -> RunnerState   --dstate (action height hurt recover)
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
      Just p -> if p < 1 then Just (clamp (p + 0.04) 0 1) else Nothing  -- rate at which jump is made (following sin wave) 
      Nothing -> Nothing
    hurt = case dsHurt ds of
      Just p -> if p < 1 then Just (clamp (p + 0.04) 0 1) else Nothing   -- time for which it'll stay hurt
      Nothing -> Nothing
    recover = case dsRecover ds of
      Just p -> if p < 1 then Just (clamp (p + 0.01) 0 1) else Nothing  -- recovery rate
      Nothing -> Nothing

stepRunnerPosition :: Step RunnerAction -> Animations RunnerKey -> Animate.Position RunnerKey Seconds -> Animate.Position RunnerKey Seconds
stepRunnerPosition (Step'Sustain _) animations pos = Animate.stepPosition animations pos frameDeltaSeconds
stepRunnerPosition (Step'Change _ da) _ _ = case da of
  RunnerAction'Move -> Animate.initPosition RunnerKey'Move
  RunnerAction'Duck -> Animate.initPosition RunnerKey'Sneak
  RunnerAction'Jump -> Animate.initPositionLoops RunnerKey'Kick 0
  RunnerAction'Hurt -> Animate.initPosition RunnerKey'Hurt

stepSpeed :: Step RunnerAction -> Percent -> Percent
stepSpeed sda speed = clamp speed' 1 20   --range of speed
  where
    da = smash sda
    speed' = case da of
      RunnerAction'Duck -> speed    -- speed'll decrease with 0.1  
      RunnerAction'Move -> speed + 0.03
      RunnerAction'Jump -> speed    -- same
      RunnerAction'Hurt -> speed - 0.15

showRunner :: RunnerState -> Bool
showRunner RunnerState{dsRecover} = case dsRecover of
  Nothing -> True
  Just percent -> sin (1000 * (percent ** 3)) >= 0

addStocks :: Score -> Score -> Bool    -- adding extra lives after some selected scores 
addStocks s s' = or [pass 1, pass 5, pass 10, pass 20, pass 35, pass 50, pass 75, powerOf50]
  where
    pass n = s < n && s' >= n
    powerOf50 = s' >= 100 && mod s 50 > mod s' 50

nextStocks :: Score -> Score -> Stocks -> Stocks   --max lives one can gather at a run is 8
nextStocks s s' st = min 10 (st + if addStocks s s' then 1 else 0)

stepZoom :: Step RunnerAction -> RunnerState -> RunnerState   --dstate (action height hurt recover)
stepZoom stepDa ds = case stepDa of
    Step'Change _ da -> case da of
      RunnerAction'Duck -> RunnerState da (Just 0) hurt recover
      RunnerAction'Hurt -> RunnerState da height (Just 0) (Just 0)
      _ -> RunnerState nextAction height hurt recover
    Step'Sustain _ -> RunnerState nextAction height hurt recover
  where
    nextAction
      | hurt /= Nothing = RunnerAction'Hurt
      | height /= Nothing = RunnerAction'Duck
      | otherwise = smash stepDa
    height = case dsHeight ds of
      Just p -> if p < 1 then Just (clamp (p + 0.04) 0 1) else Nothing  -- rate at which jump is made (following sin wave) 
      Nothing -> Nothing
    hurt = case dsHurt ds of
      Just p -> if p < 1 then Just (clamp (p + 0.04) 0 1) else Nothing   -- time for which it'll stay hurt
      Nothing -> Nothing
    recover = case dsRecover ds of
      Just p -> if p < 1 then Just (clamp (p + 0.01) 0 1) else Nothing  -- recovery rate
      Nothing -> Nothing

-- stepZoom :: Float -> RunnerAction -> Float    --zoom and zoomout speed control 
-- stepZoom zoom runnerAction = case runnerAction of
--   RunnerAction'Duck -> clamp (zoom - 0.01) 0 1   --range with rate of change
--   _ -> clamp (zoom + 0.05) 0 1

applyHurt :: Bool -> Step RunnerAction -> Maybe Percent -> Step RunnerAction
applyHurt collision stepDa recover
  | collision && recover == Nothing = case stepDa of
      Step'Sustain RunnerAction'Hurt -> stepDa
      Step'Sustain da -> Step'Change da RunnerAction'Hurt
      Step'Change da _ -> Step'Change da RunnerAction'Hurt
  | otherwise = stepDa
