module StreetRun.Quake_Street where

import StreetRun.Types
import StreetRun.Frame
import StreetRun.Step
import StreetRun.Quake

applyQuakeToStreet :: Quake -> (Int, Int) -> (Int, Int)
applyQuakeToStreet = applyQuake 4 4                         -- amount by which street would shake or vibrate