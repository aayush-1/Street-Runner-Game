{-# LANGUAGE TemplateHaskell #-}
module StreetRunner.Engine.Common where

import Control.Lens

import StreetRunner.Engine.Types
import StreetRunner.Engine.Quake
import StreetRunner.Engine.Sfx

data CommonVars = CommonVars
  { cvHiscore :: Score
  , cvQuake :: Quake
  , cvSfx :: [Sfx]
  } deriving (Show, Eq)

makeClassy ''CommonVars

initCommonVars :: CommonVars
initCommonVars = CommonVars 0 (Quake'Dormant 2) []   --displaying initial high score as 0 in the game
