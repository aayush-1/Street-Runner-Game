{-# LANGUAGE TemplateHaskell #-}
module StreetRun.Work where

import qualified Animate
import Control.Lens

import StreetRun.Runner
import StreetRun.Runner_func_1
import StreetRun.Runner_func_2
import StreetRun.Scenario
import StreetRun.Hurdle
import StreetRun.Hurdle_1
import StreetRun.Types

data PlayVars = PlayVars
  { pvScore :: Score
  , pvStocks :: Stocks
  , pvSpeed :: Percent
  , pvSeconds :: Seconds
  , pvZoom :: RunnerState
  , pvShowRunner :: Bool
  , pvRunnerPos :: Animate.Position RunnerKey Seconds
  , pvBuildingPos :: Animate.Position BuildingKey Seconds
  , pvGrassPos :: Animate.Position GrassKey Seconds
  , pvRunnerState :: RunnerState
  , pvBuildingScroll :: Distance
  , pvBackStreetScroll :: Distance
  , pvStreetScroll :: Distance
  , pvGrassScroll :: Distance
  , pvObstacles :: [ObstacleState]
  , pvUpcomingObstacles :: [(Int, ObstacleTag)]
  } deriving (Show, Eq)

