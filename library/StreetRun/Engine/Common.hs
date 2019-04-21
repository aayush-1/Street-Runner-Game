{-# LANGUAGE TemplateHaskell #-}
module StreetRun.Engine.Common where

import Control.Lens

import StreetRun.Engine.Types
import StreetRun.Engine.Quake
import StreetRun.Engine.Sfx

data CommonVars = CommonVars
  { cvHiscore :: Score
  , cvQuake :: Quake
  , cvSfx :: [Sfx]
  } deriving (Show, Eq)

makeClassy ''CommonVars

initCommonVars :: CommonVars
initCommonVars = CommonVars 0 (Quake'Dormant 2) []   --displaying initial high score as 0 in the game
