module StreetRun.Quake_Building where

import StreetRun.Types
import StreetRun.CFL
import StreetRun.Step
import StreetRun.Quake

applyQuakeToBuilding :: Quake -> (Int, Int) -> (Int, Int)
applyQuakeToBuilding = applyQuake 2 8                       -- amount by which buildings would shake or vibrate
