{-# LANGUAGE TemplateHaskell #-}
module StreetRunner.State where

import Control.Lens

import StreetRunner.Engine.Common
import StreetRunner.Engine.Scene
import StreetRunner.Engine.Input
import StreetRunner.Engine.Camera
import StreetRunner.Engine.Obstacle
import StreetRunner.Engine.Play
import StreetRunner.Engine.GameOver
import StreetRunner.Engine.Title

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
