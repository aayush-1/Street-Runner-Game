{-# LANGUAGE TemplateHaskell #-}
module StreetRun.Work_func where

import qualified Animate
import Control.Lens

import StreetRun.Runner
import StreetRun.Runner_func_1
import StreetRun.Runner_func_2
import StreetRun.Scenario
import StreetRun.Hurdle
import StreetRun.Hurdle_1
import StreetRun.Types
import StreetRun.Work

makeClassy ''PlayVars

initPlayVars :: [(Int, ObstacleTag)] -> PlayVars
initPlayVars upcomingObstacles = PlayVars
  { pvScore = 0
  , pvStocks = 3
  , pvSeconds = 0
  , pvSpeed = 1
  , pvShowRunner = True
  , pvZoom = RunnerState RunnerAction'Move Nothing Nothing Nothing
  , pvRunnerState = RunnerState RunnerAction'Move Nothing Nothing Nothing
  , pvRunnerPos = Animate.initPosition RunnerKey'Move
  , pvBuildingPos = Animate.initPosition BuildingKey'Idle
  , pvGrassPos = Animate.initPosition GrassKey'Idle
  , pvBuildingScroll = 0
  , pvBackStreetScroll = 0
  , pvStreetScroll = 0
  , pvGrassScroll = 0
  , pvObstacles = []
  , pvUpcomingObstacles = upcomingObstacles
  }
