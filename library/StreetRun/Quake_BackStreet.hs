module StreetRun.Quake_BackStreet where
import StreetRun.Types
import StreetRun.CFL
import StreetRun.Step
import StreetRun.Quake




applyQuakeToBackStreet :: Quake -> (Int, Int) -> (Int, Int)
applyQuakeToBackStreet = applyQuake 3 6                         -- amount by which back-street would shake or vibrate