{-# LANGUAGE TemplateHaskell #-}--to directly import currying functions 
module StreetRun.Tag where

import qualified Animate
import Control.Lens

import StreetRun.Types
import StreetRun.Runner
import StreetRun.Runner_func_1
import StreetRun.Runner_func_2
import StreetRun.Scenario

data TitleVars = TitleVars
  { tvRunnerPos :: Animate.Position RunnerKey Seconds 
  , tvBuildingPos :: Animate.Position BuildingKey Seconds
  , tvGrassPos :: Animate.Position GrassKey Seconds
  , tvFlashing :: Float
  } deriving (Show, Eq)

