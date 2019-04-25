module StreetRun.Clock where

import Control.Concurrent (threadDelay) --Suspends the current thread for a given number of microseconds

class Monad m => Clock m where --m is instance of monad 
  delayMilliseconds :: Int -> m ()

delayMilliseconds' :: Int -> IO ()
delayMilliseconds' ms = threadDelay (1000 * ms) -- delay in execution of different threads
