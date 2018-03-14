{-# LANGUAGE ExistentialQuantification #-}
module Test.Rufous.Options 
   ( RufousOptions(..)
   , DebugOptions(..)
   , debugFlag
   , debugOpt
   )
where

import Test.Rufous.Signature
import Test.Rufous.Profile
import Test.Rufous.DUG

data RufousOptions =
   forall d.
   RufousOptions
      { signature :: Signature
      , profiles :: [Profile]
      , dugs :: [DUG d]
      , averageDugSize :: Int
      , numberOfTests :: Int
      , debug :: Bool
      , debugOptions :: DebugOptions
      }

data DebugOptions = 
   DebugOptions {
        dumpDir :: String
      , dumpDugs :: Bool
      , dumpPhaseTiming :: Bool
      , showNullTimes :: Bool
   }

debugOpt :: (DebugOptions -> a) -> a -> RufousOptions -> a
debugOpt f x r = if debug r then f (debugOptions r) else x

debugFlag :: (DebugOptions -> Bool) -> RufousOptions -> Bool
debugFlag f r = debugOpt f False r
