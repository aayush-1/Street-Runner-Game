module StreetRun.CFL where

import Control.Concurrent (threadDelay) --Suspends the current thread for a given number of microseconds
import Data.Text (Text)

class Monad m => Clock m where --m is instance of monad 
  delayMilliseconds :: Int -> m ()

delayMilliseconds' :: Int -> IO ()
delayMilliseconds' ms = threadDelay (1000 * ms) -- delay in execution of different threads

class Monad m => Logger m where
  logText :: Text -> m ()

frameDeltaSeconds :: Fractional a => a --time it takes to change the frames 
frameDeltaSeconds = 0.016667

frameDeltaMilliseconds :: Int
frameDeltaMilliseconds = 20 --time it takes to change the frame 
