{-# LANGUAGE TemplateHaskell #-}
module StreetRun.Engine.Play where

import qualified Animate
import Control.Lens

import StreetRun.Engine.Street
import StreetRun.Engine.Building
import StreetRun.Engine.Grass
import StreetRun.Engine.Obstacle
import StreetRun.Engine.Types

data PlayVars = PlayVars
  { pvScore :: Score
  , pvStocks :: Stocks
  , pvSpeed :: Percent
  , pvSeconds :: Seconds
  , pvZoom :: Float
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

makeClassy ''PlayVars

initPlayVars :: [(Int, ObstacleTag)] -> PlayVars
initPlayVars upcomingObstacles = PlayVars
  { pvScore = 0
  , pvStocks = 3
  , pvSeconds = 0
  , pvSpeed = 1
  , pvShowRunner = True
  , pvZoom = 1
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
