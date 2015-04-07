module System.RedProx.Poison (
    PoisonRef,
    Poison (..),
    newPoisonRef,
    isPoisoned,
    poison
) where

import Data.IORef

type PoisonRef = IORef Poison
data Poison = Alive | Dead

newPoisonRef :: IO PoisonRef
newPoisonRef = newIORef Alive

isPoisoned :: PoisonRef -> IO Bool
isPoisoned poison = do
    p <- readIORef poison
    case p of
        Alive -> return False
        Dead -> return True

poison :: PoisonRef -> IO ()
poison p = writeIORef p Dead
