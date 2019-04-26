module StreetRun.Assets where

import qualified SDL
import qualified SDL.Mixer as Mixer
import qualified SDL.Font as Font
import qualified SDL.Image as Image
import qualified Animate
import Data.Text.Conversions (toText)
import Data.StateVar (($=))
import SDL.Vect
import qualified SDL.Raw.Video as Raw
import qualified SDL.Internal.Numbered as Numbered

import StreetRun.Types
import StreetRun.Runner
import StreetRun.Runner_func_1
import StreetRun.Runner_func_2
import StreetRun.Font
import StreetRun.Font_functions
import StreetRun.Snag
import StreetRun.Scenario


data Resources = Resources
  { rBuildingSprites :: Animate.SpriteSheet BuildingKey SDL.Texture Seconds
  , rGrassSprites :: Animate.SpriteSheet GrassKey SDL.Texture Seconds
  , rBackStreetSprites :: SDL.Texture
  , rStreetSprites :: SDL.Texture
  , rRunnerSprites :: Animate.SpriteSheet RunnerKey SDL.Texture Seconds
  , rBirdSprites :: Animate.SpriteSheet BirdKey SDL.Texture Seconds
  , rLavaSprites :: Animate.SpriteSheet LavaKey SDL.Texture Seconds
  , rRockSprites :: Animate.SpriteSheet RockKey SDL.Texture Seconds
  , rGameMusic :: Mixer.Music
  , rJumpSfx :: Mixer.Chunk
  , rDuckSfx :: Mixer.Chunk
  , rPointSfx :: Mixer.Chunk
  , rBirdSfx :: Mixer.Chunk
  , rHurtSfx :: Mixer.Chunk
  , rLavaSfx :: Mixer.Chunk
  , rQuakeSfx :: Mixer.Chunk
  , rRockSfx :: Mixer.Chunk
  , rDeathSfx :: Mixer.Chunk
  , rRecoverSfx :: Mixer.Chunk
  , rStockSfx :: Mixer.Chunk
  , rPauseSprite :: SDL.Texture
  , rSpaceSprite :: SDL.Texture
  , rEscapeSprite :: SDL.Texture
  , rGameOverSprite :: SDL.Texture
  , rHiscoreSprite :: SDL.Texture
  , rTitleSprite :: SDL.Texture
  , rNumberSprites :: Number -> SDL.Texture
  , rControlsSprite :: SDL.Texture
  }

-- | Produce a new 'SDL.Surface' based on an existing one, but
-- optimized for blitting to the specified 'SDL.PixelFormat'.
-- SDL_surface contains collection of pixel used 
-- SDL_PixelFormat -- format in which pixel info is stored 
convertSurface :: SDL.Surface -> SDL.PixelFormat -> IO SDL.Surface  
convertSurface (SDL.Surface s _) pixFmt = do
  fmt <- Raw.allocFormat (Numbered.toNumber pixFmt) --tonumber converts string to numbers , returns pointer to pxfmt
  surface <- SDL.Surface <$> Raw.convertSurface s fmt 0 <*> pure Nothing-- returns new surface 
  surface <$ Raw.freeFormat fmt 

loadSurface :: FilePath -> Maybe Animate.Color -> IO SDL.Surface
loadSurface path alpha = do
  surface0 <- Image.load path
  surface <- convertSurface surface0 SDL.RGBA8888
  SDL.freeSurface surface0
  case alpha of
    Just (r,g,b) -> SDL.surfaceColorKey surface $= (Just $ V4 r g b 0x00)
    Nothing -> return ()
  return surface

alphaColorDef :: Animate.Color -- backStreet color defination for transparency
alphaColorDef = (0xff,0x00,0xff)


