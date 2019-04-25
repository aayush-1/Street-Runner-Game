{-# LANGUAGE TemplateHaskell #-}
module StreetRun.State where

import Control.Lens

import StreetRun.Common
import StreetRun.Scene_change
import StreetRun.Input
import StreetRun.Camera
import StreetRun.Hurdle
import StreetRun.Hurdle_1
import StreetRun.Work
import StreetRun.Work_func
import StreetRun.Out
import StreetRun.Out_func
import StreetRun.Tag
import StreetRun.Tag_func

data Vars = Vars 
  { vCommon :: CommonVars
  , vScene :: Scene
  , vNextScene :: Scene
  , vTitle :: TitleVars
  , vPlay :: PlayVars
  , vGameOver :: GameOverVars
  , vInput :: Input
  , vCamera :: Camera
  } deriving (Show, Eq)

initVars :: [(Int, ObstacleTag)] -> Vars -- initialise all basic variables 
initVars mkObstacles = Vars initCommonVars Scene'Title Scene'Title initTitleVars (initPlayVars mkObstacles) initGameOverVars initInput initCamera

instance HasCommonVars Vars where
  commonVars = lens vCommon (\v s -> v { vCommon = s })

instance HasTitleVars Vars where
  titleVars = lens vTitle (\v s -> v { vTitle = s })

instance HasPlayVars Vars where
  playVars = lens vPlay (\v s -> v { vPlay = s })

instance HasGameOverVars Vars where
  gameOverVars = lens vGameOver (\v s -> v { vGameOver = s })

makeClassy ''Vars --to easily access nested structures
