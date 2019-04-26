{-# LANGUAGE TemplateHaskell #-}--to directly import currying functions 
module StreetRun.Tag_func where

import qualified Animate
import Control.Lens
import StreetRun.Tag 

import StreetRun.Types
import StreetRun.Runner
import StreetRun.Runner_func_1
import StreetRun.Runner_func_2
import StreetRun.Scenario



makeClassy ''TitleVars

initTitleVars :: TitleVars
initTitleVars = TitleVars (Animate.initPosition RunnerKey'Idle) (Animate.initPosition BuildingKey'Idle) (Animate.initPosition GrassKey'Idle) 0

titleShowPressSpace :: Float -> Bool
titleShowPressSpace x = sin x > 0.5

titleShowPressEscape :: Float -> Bool
titleShowPressEscape p = sin p < -0.5

