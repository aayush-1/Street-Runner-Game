module StreetRunner.Engine.Sfx where

data Sfx          --data types for special effects for different tasks by the runner
  = Sfx'Jump
  | Sfx'Point
  | Sfx'Duck
  | Sfx'Bird
  | Sfx'Hurt
  | Sfx'Lava
  | Sfx'Quake
  | Sfx'Rock
  | Sfx'Recover
  | Sfx'Stock
  deriving (Show, Eq)
