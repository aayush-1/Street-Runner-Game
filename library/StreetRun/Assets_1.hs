module StreetRun.Assets_1 where

import StreetRun.Assets
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
