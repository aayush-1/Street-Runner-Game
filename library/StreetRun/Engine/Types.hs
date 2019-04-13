module StreetRun.Engine.Types where

import qualified Animate

import Data.Aeson (FromJSON, ToJSON)

type Animations key = Animate.Animations key (Animate.SpriteClip key) Seconds
type DrawSprite key m = Animate.SpriteClip key -> (Int, Int) -> m ()

newtype Stocks = Stocks Int --stock type used exclusively as Int 
  deriving (Show, Eq, Num, Integral, Real, Ord, Enum)

newtype Percent = Percent Float --percent type used exclusively as Flaot
  deriving (Show, Eq, Num, Fractional, Floating, RealFrac, Real, Ord)

newtype Distance = Distance Float --Distance type used exclusively as Float
  deriving (Show, Eq, Num, Fractional, RealFrac, Real, Ord)

newtype Seconds = Seconds Float --Seconds type used exclusively as Float
  deriving (Show, Eq, Num, ToJSON, FromJSON, Fractional, Ord)

newtype Score = Score Int --Score type used exclusively as Int
  deriving (Show, Eq, Num, Integral, Real, Ord, Enum)

clamp :: (Fractional a, Ord a) => a -> a -> a -> a  --a should be a instance of Ord and Fractional typeclass 
clamp present mn mx = if present > mx then mx else (if present < mn then mn else present) -- clamping function 
