{-# LANGUAGE TemplateHaskell #-}
module StreetRun.State where

import Control.Lens

import StreetRun.Engine.Common
import StreetRun.Engine.Scene
import StreetRun.Engine.Input
import StreetRun.Engine.Camera
import StreetRun.Engine.Obstacle
import StreetRun.Engine.Play
import StreetRun.Engine.GameOver
import StreetRun.Engine.Title

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
