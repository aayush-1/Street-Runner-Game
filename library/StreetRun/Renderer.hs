module StreetRun.Renderer where

import qualified Animate
import qualified SDL
import Data.StateVar (($=))--readable writable values of state variable 
import Foreign.C.Types
import SDL.Vect
import Control.Monad.Reader

import StreetRun.Config
import StreetRun.Types
import StreetRun.Runner
import StreetRun.Snag
import StreetRun.Font
import StreetRun.Font_functions
import StreetRun.Scenario
import StreetRun.SDLLayer

class Monad m => Renderer m where
  clearScreen :: m ()
  drawScreen :: m ()
  getRunnerAnimations :: m (Animations RunnerKey)
  getLavaAnimations :: m (Animations LavaKey)
  getRockAnimations :: m (Animations RockKey)
  getBirdAnimations :: m (Animations BirdKey)
  getBuildingAnimations :: m (Animations BuildingKey)
  getGrassAnimations :: m (Animations GrassKey)
  drawRunner :: DrawSprite RunnerKey m
  drawLava :: DrawSprite LavaKey m
  drawRock :: DrawSprite RockKey m
  drawBird :: DrawSprite BirdKey m
  drawBuilding :: DrawSprite BuildingKey m
  drawGrass :: DrawSprite GrassKey m
  drawBackStreet :: (Int, Int) -> m ()
  drawStreet :: (Int, Int) -> m ()
  drawBlackOverlay :: Percent -> m ()
  drawHiscoreText :: (Int, Int) -> m ()
  drawPauseText :: (Int, Int) -> m ()
  drawGameOverText :: (Int, Int) -> m ()
  drawPressSpaceText :: (Int, Int) -> m ()
  drawPressEscapeText :: (Int, Int) -> m ()
  drawTitleText :: (Int, Int) -> m ()
  drawNumber :: Number -> (Int, Int) -> m ()
  drawControlsText :: (Int, Int) -> m ()

clearScreen' :: (SDLRenderer m, MonadReader Config m) => m () --MonadReader Config is used to avoid accessing child functions again and again
clearScreen' = do 
  renderer <- asks cRenderer --takes the value of cRenderer
  clearRenderer renderer

drawScreen' :: (SDLRenderer m, MonadReader Config m) => m ()
drawScreen' = do
  renderer <- asks cRenderer
  presentRenderer renderer --Update the screen with any rendering performed since the previous call as it works on backbuffer 


buildingY, backStreetY, streetY, grassY :: Int             
buildingY = -16
backStreetY = 16 * 0
streetY = 16 * 28
grassY = 16 * 36



