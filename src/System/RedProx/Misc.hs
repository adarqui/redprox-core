module System.RedProx.Misc (
    notM,
    delay
) where

import Control.Concurrent

notM :: Bool -> IO Bool
notM True = return False
notM False = return True

delay :: IO ()
delay = threadDelay 1000000
