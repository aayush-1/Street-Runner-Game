module StreetRun.Quake_Grass where

import StreetRun.Types
import StreetRun.CFL
import StreetRun.Step
import StreetRun.Quake

applyQuakeToGrass :: Quake -> (Int, Int) -> (Int, Int)
applyQuakeToGrass = applyQuake 5 3                          -- amount by which grass would shake or vibrate