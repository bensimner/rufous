module Test.Rufous.Options 
   ( RufousOptions(..)
   , defaultOptions
   )
where

data RufousOptions =
   RufousOptions
      { dumpDugs :: Bool
      , dumpDir :: String
      , averageDugSize :: Int
      , numberOfTests :: Int
      }
   deriving (Eq, Show)

defaultOptions = RufousOptions { dumpDugs=False, dumpDir="./", averageDugSize=10, numberOfTests=1 }
