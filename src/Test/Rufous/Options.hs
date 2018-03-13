module Test.Rufous.Options 
   ( RufousOptions(..)
   , DebugOptions(..)
   , defaultOptions
   , defaultDebugOptions
   , debugFlag
   , debugOpt
   )
where

data RufousOptions =
   RufousOptions
      { averageDugSize :: Int
      , numberOfTests :: Int
      , debug :: Bool
      , debugOptions :: DebugOptions
      }
   deriving (Eq, Show)

data DebugOptions = 
   DebugOptions {
        dumpDir :: String
      , dumpDugs :: Bool
      , dumpPhaseTiming :: Bool
   }
   deriving (Eq, Show)

defaultDebugOptions = DebugOptions { dumpDugs=False, dumpDir="./", dumpPhaseTiming=True }
defaultOptions = RufousOptions { averageDugSize=10, numberOfTests=1, debug=False, debugOptions=defaultDebugOptions }

debugOpt :: (DebugOptions -> a) -> a -> RufousOptions -> a
debugOpt f x r = if debug r then f (debugOptions r) else x

debugFlag :: (DebugOptions -> Bool) -> RufousOptions -> Bool
debugFlag f r = debugOpt f False r
