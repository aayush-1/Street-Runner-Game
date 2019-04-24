module StreetRun.Effect.HUD where

import Control.Lens (view)
import Control.Monad (when)
import Control.Monad.State (MonadState, gets)

import StreetRun.Engine.Common
import StreetRun.Engine.Play
import StreetRun.Engine.Font
import StreetRun.Effect.Renderer

class Monad m => HUD m where
  drawHiscore :: m ()
  drawScore :: m ()
  drawControls :: m ()

drawHiscore' :: (Renderer m, MonadState s m, HasCommonVars s) => m ()
drawHiscore' = do
  cv <- gets (view commonVars)
  drawHiscoreText (1180, 16) --Position of Record in the window (x and y)
  drawNumbers (fromIntegral $ cvHiscore cv) (1234, 50) --record number position 

drawScore' :: (Renderer m, MonadState s m, HasPlayVars s) => m ()
drawScore' = do
  pv <- gets (view playVars)
  drawNumbers (fromIntegral $ pvScore pv) (1234, 100) -- score position

drawControls' :: (Renderer m, MonadState s m, HasPlayVars s) => m ()
drawControls' = do
  pv <- gets (view playVars)
  when (pvSeconds pv < 0) $ drawControlsText (200,470) 

drawNumbers :: Renderer m => Integer -> (Int, Int) -> m ()
drawNumbers int (x,y) = mapM_
  (\(i, n) -> drawNumber n (x - i * 16, y))
  (zip [0..] (toNumberReverse int))
