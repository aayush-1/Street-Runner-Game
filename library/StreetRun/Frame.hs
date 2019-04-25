module StreetRun.Frame where

frameDeltaSeconds :: Fractional a => a --time it takes to change the frames 
frameDeltaSeconds = 0.016667

frameDeltaMilliseconds :: Int
frameDeltaMilliseconds = 20 --time it takes to change the frame 
