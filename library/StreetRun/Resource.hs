module StreetRun.Resource where

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

loadResources :: SDL.Renderer -> IO Resources
loadResources renderer = do
  smallFont <- Font.load "resource/Computer Speak v0.3.ttf" 24
  bigFont <- Font.load "resource/Computer Speak v0.3.ttf" 64
  titleFont <- Font.load "resource/Computer Speak v0.3.ttf" 100
  gameMusic <- Mixer.load "resource/v42.mod"
  jumpSfx <- Mixer.load "resource/jump.wav"
  pointSfx <- Mixer.load "resource/point.wav"
  birdSfx <- Mixer.load "resource/bird.wav"
  duckSfx <- Mixer.load "resource/duck.wav"
  hurtSfx <- Mixer.load "resource/hurt.wav"
  quakeSfx <- Mixer.load "resource/quake.wav"
  rockSfx <- Mixer.load "resource/rock.wav"
  lavaSfx <- Mixer.load "resource/lava.wav"
  deathSfx <- Mixer.load "resource/death.wav"
  stockSfx <- Mixer.load "resource/stock.wav"
  recoverSfx <- Mixer.load "resource/recover.wav"
  buildingSprites <- Animate.readSpriteSheetJSON loadTexture "resource/building.json" :: IO (Animate.SpriteSheet BuildingKey SDL.Texture Seconds)
  --function for loading SpriteSheetInfo, then using it to load its image for a SpriteSheet
 
  backStreet <- loadTexture "resource/backStreet.png" (Just alphaColorDef)
  street <- loadTexture "resource/street.png" (Just alphaColorDef)
  controlsSprite <- loadTexture "resource/controls.png" (Just alphaColorDef)
  grassSprites <- Animate.readSpriteSheetJSON loadTexture "resource/grass.json" :: IO (Animate.SpriteSheet GrassKey SDL.Texture Seconds)
 
  pauseSprite <- toTexture =<< Font.blended bigFont (V4 200 200 200 200) "PAUSED :)"
  spaceSprite <- toTexture =<< Font.blended smallFont (V4 255 255 255 255) "PRESS SPACE TO RUN"
  escapeSprite <- toTexture =<< Font.blended smallFont (V4 255 255 255 255) "PRESS ESCAPE TO QUIT"
  gameOverSprite <- toTexture =<< Font.blended bigFont (V4 255 255 255 255) "GAME OVER :(("
  hiscoreSprite <- toTexture =<< Font.blended smallFont (V4 255 255 255 255) "Record"
  titleSprite <- toTexture =<< Font.blended titleFont  (V4 0x99 0x99 0x99 0x99) " STREET RUNNER "
  let drawFont :: Int -> IO SDL.Texture
      drawFont n = toTexture =<< Font.blended smallFont (V4 255 255 255 255) (toText $ show n)
  num0 <- drawFont 0
  num1 <- drawFont 1
  num2 <- drawFont 2
  num3 <- drawFont 3
  num4 <- drawFont 4
  num5 <- drawFont 5
  num6 <- drawFont 6
  num7 <- drawFont 7
  num8 <- drawFont 8
  num9 <- drawFont 9
  let numberSprites = \n -> case n of
        Number'0 -> num0
        Number'1 -> num1
        Number'2 -> num2
        Number'3 -> num3
        Number'4 -> num4
        Number'5 -> num5
        Number'6 -> num6
        Number'7 -> num7
        Number'8 -> num8
        Number'9 -> num9
  runnerSprites <- Animate.readSpriteSheetJSON loadTexture "resource/runner.json" :: IO (Animate.SpriteSheet RunnerKey SDL.Texture Seconds)
  birdSprites <- Animate.readSpriteSheetJSON loadTexture "resource/bird.json" :: IO (Animate.SpriteSheet BirdKey SDL.Texture Seconds)
  lavaSprites <- Animate.readSpriteSheetJSON loadTexture "resource/lava.json" :: IO (Animate.SpriteSheet LavaKey SDL.Texture Seconds)
  rockSprites <- Animate.readSpriteSheetJSON loadTexture "resource/rock.json" :: IO (Animate.SpriteSheet RockKey SDL.Texture Seconds)
  Font.free smallFont
  Font.free bigFont
  Font.free titleFont
  return Resources
    { rBuildingSprites = buildingSprites
    , rBackStreetSprites = backStreet
    , rStreetSprites = street
    , rGrassSprites = grassSprites
    , rRunnerSprites = runnerSprites
    , rBirdSprites = birdSprites
    , rLavaSprites = lavaSprites
    , rRockSprites = rockSprites
    , rJumpSfx = jumpSfx
    , rDuckSfx = duckSfx
    , rPointSfx = pointSfx
    , rBirdSfx = birdSfx
    , rHurtSfx = hurtSfx
    , rLavaSfx = lavaSfx
    , rQuakeSfx = quakeSfx
    , rRockSfx = rockSfx
    , rDeathSfx = deathSfx
    , rStockSfx = stockSfx
    , rRecoverSfx = recoverSfx
    , rGameMusic = gameMusic
    , rPauseSprite = pauseSprite
    , rSpaceSprite = spaceSprite
    , rEscapeSprite = escapeSprite
    , rGameOverSprite = gameOverSprite
    , rHiscoreSprite = hiscoreSprite
    , rTitleSprite = titleSprite
    , rNumberSprites = numberSprites
    , rControlsSprite = controlsSprite
    }
  where
    toTexture surface = SDL.createTextureFromSurface renderer surface
    loadTexture path c = SDL.createTextureFromSurface renderer =<< loadSurface path c

freeResources :: Resources -> IO ()
freeResources r = do
  SDL.destroyTexture $ Animate.ssImage (rBuildingSprites r)
  SDL.destroyTexture (rBackStreetSprites r)
  SDL.destroyTexture (rStreetSprites r)
  SDL.destroyTexture $ Animate.ssImage (rGrassSprites r)
  SDL.destroyTexture $ Animate.ssImage (rRunnerSprites r)
  SDL.destroyTexture $ Animate.ssImage (rBirdSprites r)
  SDL.destroyTexture $ Animate.ssImage (rLavaSprites r)
  SDL.destroyTexture $ Animate.ssImage (rRockSprites r)
  SDL.destroyTexture (rPauseSprite r)
  SDL.destroyTexture (rEscapeSprite r)
  SDL.destroyTexture (rSpaceSprite r)
  SDL.destroyTexture (rGameOverSprite r)
  SDL.destroyTexture (rHiscoreSprite r)
  SDL.destroyTexture (rTitleSprite r)
  SDL.destroyTexture (rControlsSprite r)

  Mixer.free (rGameMusic r)
  Mixer.free (rJumpSfx r)
  Mixer.free (rDuckSfx r)
  Mixer.free (rPointSfx r)
  Mixer.free (rBirdSfx r)
  Mixer.free (rHurtSfx r)
  Mixer.free (rLavaSfx r)
  Mixer.free (rQuakeSfx r)
  Mixer.free (rRockSfx r)
  Mixer.free (rRecoverSfx r)
  Mixer.free (rStockSfx r)
  Mixer.free (rDeathSfx r)
