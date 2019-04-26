module StreetRun.Renderer_func_1 where

import qualified Animate
import qualified SDL
import Data.StateVar (($=))--readable writable values of state variable 
import Foreign.C.Types
import SDL.Vect
import Control.Monad.Reader
import StreetRun.Renderer
import StreetRun.Config
import StreetRun.Types
import StreetRun.Runner
import StreetRun.Snag
import StreetRun.Font
import StreetRun.Font_functions
import StreetRun.Scenario
import StreetRun.SDLLayer

drawTextureSprite :: (SDLRenderer m, MonadReader Config m) => (Config -> SDL.Texture) -> (Int, Int) -> m ()
drawTextureSprite getTex (x,y) = do
  renderer <- asks cRenderer
  tex <- asks getTex
  SDL.TextureInfo{textureWidth,textureHeight} <- queryTexture tex
  let dim = V2 textureWidth textureHeight
  drawTexture
    renderer
    tex
    Nothing
    (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y)) dim)

drawSprite :: (SDLRenderer m, MonadReader Config m) => (Config -> Animate.SpriteSheet key SDL.Texture Seconds) -> Animate.SpriteClip key -> (Int, Int) -> m ()
drawSprite ss clip (x,y) = do
  renderer <- asks cRenderer
  sheet <- asks (Animate.ssImage . ss) --sheet is collected 
  let clip'@(SDL.Rectangle _ dim) = rectFromClip clip
  drawTexture
    renderer
    sheet
    (Just clip') --copy from renderer to sheet in just clip' part of the window
    (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y)) dim)

rectFromClip :: Animate.SpriteClip key -> SDL.Rectangle CInt --same dimensions as that of sprite sheet
rectFromClip Animate.SpriteClip{scX,scY,scW,scH} = SDL.Rectangle (SDL.P (V2 (num scX) (num scY))) (V2 (num scW) (num scH))
  where
    num = fromIntegral