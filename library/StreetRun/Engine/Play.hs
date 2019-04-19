{-# LANGUAGE TemplateHaskell #-}
module StreetRunner.Engine.Play where

import qualified Animate
import Control.Lens

import StreetRunner.Engine.Street
import StreetRunner.Engine.Building
import StreetRunner.Engine.Grass
import StreetRunner.Engine.Obstacle
import StreetRunner.Engine.Types

data PlayVars = PlayVars
  { pvScore :: Score
  , pvStocks :: Stocks
  , pvSpeed :: Percent
  , pvSeconds :: Seconds
  , pvZoom :: Float
  , pvShowStreet :: Bool
  , pvStreetPos :: Animate.Position StreetKey Seconds
  , pvBuildingPos :: Animate.Position BuildingKey Seconds
  , pvGrassPos :: Animate.Position GrassKey Seconds
  , pvStreetState :: StreetState
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
  , pvShowStreet = True
  , pvZoom = 1
  , pvStreetState = StreetState StreetAction'Move Nothing Nothing Nothing
  , pvStreetPos = Animate.initPosition StreetKey'Move
  , pvBuildingPos = Animate.initPosition BuildingKey'Idle
  , pvGrassPos = Animate.initPosition GrassKey'Idle
  , pvBuildingScroll = 0
  , pvBackStreetScroll = 0
  , pvStreetScroll = 0
  , pvGrassScroll = 0
  , pvObstacles = []
  , pvUpcomingObstacles = upcomingObstacles
  }
