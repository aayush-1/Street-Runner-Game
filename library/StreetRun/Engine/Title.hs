{-# LANGUAGE TemplateHaskell #-}--to directly import currying functions 
module StreetRun.Engine.Title where

import qualified Animate
import Control.Lens

import StreetRun.Engine.Types
import StreetRun.Engine.Runner
import StreetRun.Engine.Building
import StreetRun.Engine.Grass

data TitleVars = TitleVars
  { tvRunnerPos :: Animate.Position RunnerKey Seconds 
  , tvBuildingPos :: Animate.Position BuildingKey Seconds
  , tvGrassPos :: Animate.Position GrassKey Seconds
  , tvFlashing :: Float
  } deriving (Show, Eq)

makeClassy ''TitleVars

initTitleVars :: TitleVars
initTitleVars = TitleVars (Animate.initPosition RunnerKey'Idle) (Animate.initPosition BuildingKey'Idle) (Animate.initPosition GrassKey'Idle) 0

titleShowPressSpace :: Float -> Bool
titleShowPressSpace x = sin x > 0.5

titleShowPressEscape :: Float -> Bool
titleShowPressEscape p = sin p < -0.5