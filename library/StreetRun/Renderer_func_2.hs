module StreetRun.Renderer_func_2 where

import qualified Animate
import qualified SDL
import Data.StateVar (($=))--readable writable values of state variable 
import Foreign.C.Types
import SDL.Vect
import Control.Monad.Reader
import StreetRun.Renderer_func_1
import StreetRun.Config
import StreetRun.Types
import StreetRun.Runner
import StreetRun.Lava
import StreetRun.Rock
import StreetRun.Bird
import StreetRun.Font
import StreetRun.Font_functions
import StreetRun.Building
import StreetRun.Grass
import StreetRun.SDLRenderer

drawHorizontalScrollSprite :: (MonadReader Config m, SDLRenderer m) => (Config ->  Animate.SpriteSheet key SDL.Texture Seconds) -> Int -> Animate.SpriteClip key -> (Int, Int) -> m ()
drawHorizontalScrollSprite ss scale clip (x,y) = do
  renderer <- asks cRenderer
  sheet <- asks (Animate.ssImage . ss)
  let clip'@(SDL.Rectangle _ dim) = rectFromClip clip
  let dim' = fromIntegral scale *^ dim
  drawTexture renderer sheet (Just clip') (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral x - 1280) (fromIntegral y)) dim')
  drawTexture renderer sheet (Just clip') (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y)) dim')
  drawTexture renderer sheet (Just clip') (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral x + 1280) (fromIntegral y)) dim')

getSpriteAnimations :: (MonadReader Config m) => (Config -> Animate.SpriteSheet key SDL.Texture Seconds) -> m (Animations key)
getSpriteAnimations ss = asks (Animate.ssAnimations . ss)

drawHorizontalScrollImage :: (MonadReader Config m, SDLRenderer m) => (Config -> SDL.Texture) -> Int -> (Int, Int) -> m ()
drawHorizontalScrollImage getTex scale (x,y) = do
  renderer <- asks cRenderer
  tex <- asks getTex
  SDL.TextureInfo{textureWidth,textureHeight} <- queryTexture tex
  let dim = SDL.V2 textureWidth textureHeight
  let dim' = fromIntegral scale *^ dim
  drawTexture renderer tex Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral x - 1280) (fromIntegral y)) dim')
  drawTexture renderer tex Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y)) dim')
  drawTexture renderer tex Nothing (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral x + 1280) (fromIntegral y)) dim')

drawBlackOverlay' :: (MonadReader Config m, SDLRenderer m, MonadIO m) => Percent -> m ()
drawBlackOverlay' (Percent percent) = do
  renderer <- asks cRenderer
  SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend --set the blend mode used for drawing operations
  SDL.rendererDrawColor renderer $= (V4 0 0 0 (truncate $ 255 * percent)) --set the color used for drawing operations
  SDL.fillRect renderer Nothing
  SDL.rendererDrawBlendMode renderer $= SDL.BlendNone
--


stepHorizontalDistance :: Distance -> Distance -> Distance --scene related   
stepHorizontalDistance dist speed = if dist' <= -1280 then dist' + 1280 else dist'
  where
    dist' = dist + speed
