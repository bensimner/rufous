module Test.Rufous.Internal.Timing(
     Timer
   , time
   , collect
   , reset
   )
where

import Prelude hiding (lookup)

import System.IO.Unsafe
import Data.IORef

import Data.Time.Clock
import Data.Map

data Timer = 
   Timer
      { name :: String
      , deltas :: [NominalDiffTime]
      }
   deriving (Show)

timersRef :: IORef (Map String Timer)
timersRef = unsafePerformIO $ newIORef empty
{-# NOINLINE timersRef #-}

time :: String -> IO a -> IO a
time timer x = do
   t1 <- getCurrentTime
   y <- x
   t2 <- getCurrentTime
   let d = diffUTCTime t2 t1
   oldTimers <- readIORef timersRef
   let oldTimerMayb = lookup timer oldTimers
   case oldTimerMayb of
      Just oldTimer -> do
         let newTimer = oldTimer { deltas=deltas oldTimer ++ [d] }
         let newTimers = insert timer newTimer oldTimers
         writeIORef timersRef newTimers
      Nothing -> do
         let newTimer = Timer { name=timer, deltas=[d] }
         let newTimers = insert timer newTimer oldTimers
         writeIORef timersRef newTimers
   return y

collect :: IO [(String, NominalDiffTime)]
collect = do
   timers <- readIORef timersRef
   return [(k, sum (deltas o)) | (k, o) <- toList timers]

reset :: IO ()
reset = writeIORef timersRef empty
