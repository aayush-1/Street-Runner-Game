module StreetRun.Hurdle_1 where

import qualified Animate
import System.Random
import Linear (V2(..))
import StreetRun.Hurdle
import StreetRun.Types -- to import new types defined in that module
import StreetRun.Bird
import StreetRun.Lava
import StreetRun.Rock
import StreetRun.Physics


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
