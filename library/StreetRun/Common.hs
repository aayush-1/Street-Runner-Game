{-# LANGUAGE TemplateHaskell #-}
module StreetRun.Common where

import Control.Lens

import StreetRun.Types
import StreetRun.Quake
import StreetRun.Quake_Building
import StreetRun.Quake_BackStreet
import StreetRun.Quake_Street
import StreetRun.Quake_Grass
import StreetRun.Sfx

data CommonVars = CommonVars
  { cvHiscore :: Score
  , cvQuake :: Quake
  , cvSfx :: [Sfx]
  } deriving (Show, Eq)

makeClassy ''CommonVars

initCommonVars :: CommonVars
initCommonVars = CommonVars 0 (Quake'Dormant 2) []   --displaying initial high score as 0 in the game
