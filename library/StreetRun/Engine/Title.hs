{-# LANGUAGE TemplateHaskell #-}--to directly import currying functions 
module StreetRun.Engine.Title where

import qualified Animate
import Control.Lens

import StreetRun.Engine.Types
import StreetRun.Engine.Dino
import StreetRun.Engine.Mountain
import StreetRun.Engine.River

data TitleVars = TitleVars
  { tvRunnerPos :: Animate.Position RunnerKey Seconds 
  , tvMountainPos :: Animate.Position MountainKey Seconds
  , tvRiverPos :: Animate.Position RiverKey Seconds
  , tvFlashing :: Float
  } deriving (Show, Eq)

makeClassy ''TitleVars

initTitleVars :: TitleVars
initTitleVars = TitleVars (Animate.initPosition RunnerKey'Idle) (Animate.initPosition MountainKey'Idle) (Animate.initPosition RiverKey'Idle) 0

titleShowPressSpace :: Float -> Bool
titleShowPressSpace x = sin x > 0.5

titleShowPressEscape :: Float -> Bool
titleShowPressEscape p = sin p < -0.5
