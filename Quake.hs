module StreetRunner.Engine.Quake where

import StreetRunner.Engine.Types
import StreetRunner.Engine.Frame
import StreetRunner.Engine.Step

data Quake
  = Quake'Progress Percent
  | Quake'Dormant Seconds
  deriving (Show, Eq)

stepQuake :: Quake -> Step Quake
stepQuake q@(Quake'Progress p)
  | p' >= 1 = Step'Change q (Quake'Dormant 30)
  | otherwise = Step'Sustain (Quake'Progress p')
  where
    p' = p + 0.02
stepQuake q@(Quake'Dormant s)
  | s' <= 0 = Step'Change q (Quake'Progress 0)
  | otherwise = Step'Sustain (Quake'Dormant s')
  where
    s' = s - frameDeltaSeconds

startQuake :: Step Quake -> Bool
startQuake (Step'Change _ (Quake'Progress _)) = True
startQuake _ = False

quakeAdjust :: Int -> Int -> Quake -> (Int, Int)
quakeAdjust rate scale (Quake'Progress p) =
  ( truncate $ cos (p * 4 * pi * fromIntegral rate) * fromIntegral scale
  , truncate $ sin (p * 2 * pi * fromIntegral rate) * fromIntegral scale )
quakeAdjust _ _ _ = (0,0)

applyQuake :: Int -> Int -> Quake -> (Int, Int) -> (Int, Int)
applyQuake rate scale q (x,y) = (x + x', y + y')
  where
    (x', y') = quakeAdjust rate scale q

applyQuakeToBuilding :: Quake -> (Int, Int) -> (Int, Int)
applyQuakeToBuilding = applyQuake 2 8                       -- amount by which buildings would shake or vibrate

applyQuakeToBackStreet :: Quake -> (Int, Int) -> (Int, Int)
applyQuakeToBackStreet = applyQuake 3 6                         -- amount by which back-street would shake or vibrate

applyQuakeToStreet :: Quake -> (Int, Int) -> (Int, Int)
applyQuakeToStreet = applyQuake 4 4                         -- amount by which street would shake or vibrate

applyQuakeToGrass :: Quake -> (Int, Int) -> (Int, Int)
applyQuakeToGrass = applyQuake 5 3                          -- amount by which grass would shake or vibrate
