module StreetRun.Engine.Obstacle where

import qualified Animate
import System.Random
import Linear (V2(..))

import StreetRun.Engine.Types -- to import new types defined in that module
import StreetRun.Engine.Bird
import StreetRun.Engine.Lava
import StreetRun.Engine.Rock
import StreetRun.Engine.Physics

data ObstacleTag
  = ObstacleTag'Lava    --nullary constructors 
  | ObstacleTag'Rock
  | ObstacleTag'Bird
  deriving (Show, Eq, Ord, Bounded, Enum) --hence deriving Enum 

instance Random ObstacleTag where
  randomR = randomRBoundedEnum
  random g = randomR (minBound, maxBound) g

data ObstacleInfo
  = ObstacleInfo'Lava (Animate.Position LavaKey Seconds) --seconds will function just like float (defined in types in module)
  | ObstacleInfo'Rock (Animate.Position RockKey Seconds) 
  | ObstacleInfo'Bird (Animate.Position BirdKey Seconds)
  deriving (Show, Eq)

data ObstacleState = ObstacleState
  { osInfo :: ObstacleInfo
  , osDistance :: Distance -- again defined in types 
  } deriving (Show, Eq)

lavaY, rockY, birdY :: Num a => a -- dist of obstacle from top 
lavaY = 16 * 28
rockY = 16 * 26 + 2
birdY = 16 * 22   

obstacleAabb :: ObstacleState -> Aabb -- affects collision of obstacles with runner 
obstacleAabb ObstacleState{osInfo,osDistance} = case osInfo of
  ObstacleInfo'Lava _ -> Aabb (V2 (0 + dist) lavaY) (V2 (32 + dist) (lavaY + 32))
  ObstacleInfo'Rock _ -> Aabb (V2 (0 + dist) rockY) (V2 (32 + dist) (rockY + 32))
  ObstacleInfo'Bird _ -> Aabb (V2 (0 + dist) birdY) (V2 (32 + dist) (birdY + 32)) --(obleft obtop)(obright obdown)
  where
    dist = realToFrac osDistance

randomRBoundedEnum :: (Bounded a, Enum a, RandomGen g) => (a, a) -> g -> (a, g)
randomRBoundedEnum (aMin, aMax) g = let
  (index, g') = randomR (fromEnum aMin, fromEnum aMax) g
  lastEnum = maxBound
  a = [minBound..lastEnum] !! (index `mod` (fromEnum lastEnum + 1))
  in (a, g')

streamOfObstacles :: RandomGen g => g -> [(Int, ObstacleTag)]
streamOfObstacles g = zip (map (\dist -> dist `mod` 18+3) $ randoms g) (randoms g) -- dist%18 captures distance between obstacles arriving 

stepObstacles :: Distance -> [ObstacleState] -> [ObstacleState] --speed of bird flying
stepObstacles delta = map
  (\o@ObstacleState{osInfo, osDistance} -> o { osDistance = osDistance - (case osInfo of ObstacleInfo'Bird _ -> delta + 4; _ -> delta) })

removeOutOfBoundObstacles :: [ObstacleState] -> ([ObstacleState], [ObstacleState])
removeOutOfBoundObstacles os = foldr
  (\o@ObstacleState{osDistance} (removed, remained) ->
    if inBounds osDistance
      then (o : removed, remained)
      else (removed, o : remained))
  ([], [])
  os
  where
    inBounds x = x + 32 < 0

placeObstacle  :: (Int, ObstacleTag) -> ObstacleState
placeObstacle (idxDist, obsTag) = ObstacleState
  { osInfo = case obsTag of
      ObstacleTag'Lava -> ObstacleInfo'Lava (Animate.initPosition LavaKey'Idle)
      ObstacleTag'Rock -> ObstacleInfo'Rock (Animate.initPosition RockKey'Idle)
      ObstacleTag'Bird -> ObstacleInfo'Bird (Animate.initPosition BirdKey'Idle)
  , osDistance = realToFrac (idxDist * 32) + realToFrac arenaWidth
  }

lastObstacleDistance :: [ObstacleState] -> Distance
lastObstacleDistance os = maximum $ (realToFrac arenaWidth - 1) : map osDistance os

canAddObstacle :: Distance -> Bool --condition to check if obstacle can be added 
canAddObstacle dist = dist < realToFrac arenaWidth

iterateObstacles :: [(Int, ObstacleTag)] -> Percent -> [ObstacleState] -> ([ObstacleState], Int, [(Int, ObstacleTag)], Maybe ObstacleTag)
iterateObstacles upcomingObstacles speed obstacles = let
  (removed, remained) = removeOutOfBoundObstacles $ stepObstacles (realToFrac speed) obstacles
  newObstacle = if canAddObstacle (lastObstacleDistance remained)
    then let
      pair = head $ upcomingObstacles
      in Just (snd pair, placeObstacle pair)
    else Nothing
  (upcomingObstacles', obstacles') = case fmap snd newObstacle of
    Nothing -> (upcomingObstacles, remained)
    Just obstacle -> (tail $ upcomingObstacles, obstacle : remained)
  in (obstacles', length removed, upcomingObstacles', fmap fst newObstacle)
