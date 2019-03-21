{-# LANGUAGE ExistentialQuantification #-}
module Test.Rufous.Options
   ( RufousOptions(..)
   , DebugOptions(..)

   -- | Aggregation Options
   , AggregatorType(..)
   , AggregationOptions(..)

   -- | Aggreagtors
   , KMeansOptions(..)

   -- | Mutators over Options
   , debugFlag
   , debugOpt

   -- | default option sets
   , debugArgs
   , args
   , kmeansArgs
   , aggregationArgs


   -- | Option-dependent functions
   , doIf
   , verboseTrace
   , debugTrace
   )
where

import qualified Test.Rufous.Signature as S
import qualified Test.Rufous.Profile as P
import qualified Test.Rufous.DUG as D

import Test.Rufous.Internal.Aggregation.Types

data RufousOptions =
   RufousOptions
      { signature :: S.Signature
      , profiles :: [P.Profile]
      , dugs :: [D.DUG]
      , averageDugSize :: Int
      , numberOfTests :: Int
      , debug :: Bool
      , debugOptions :: DebugOptions
      , genOptions :: GenOptions
      , aggregationOptions :: AggregationOptions
      , aggregator :: AggregatorType
      , verbose :: Bool
      }

data DebugOptions =
   DebugOptions {
        dumpDir :: String
      , dumpDugs :: Bool
      , dumpPhaseTiming :: Bool
      , showNullTimes :: Bool
   }

data GenOptions =
   GenOptions

debugOpt :: (DebugOptions -> a) -> a -> RufousOptions -> a
debugOpt f x r = if debug r then f (debugOptions r) else x

debugFlag :: (DebugOptions -> Bool) -> RufousOptions -> Bool
debugFlag f r = debugOpt f False r

{- Default Options -}
genArgs :: GenOptions
genArgs = GenOptions

debugArgs :: DebugOptions
debugArgs =
   DebugOptions
      { dumpDugs=False
      , dumpDir="./"
      , dumpPhaseTiming=True
      , showNullTimes=True
      }

args :: RufousOptions
args =
   RufousOptions
      { signature=error "args :: no signature specified"
      , profiles=[]
      , dugs=[]
      , averageDugSize=1000
      , numberOfTests=100
      , debug=False
      , debugOptions=debugArgs
      , genOptions=genArgs
      , aggregator=KMeans
      , aggregationOptions=aggregationArgs
      , verbose=False
      }


{- Option-dependent Tracing Functions -}
doIf :: (RufousOptions -> Bool) -> RufousOptions -> IO () -> IO ()
doIf f opts a | f opts = a
doIf _ _ _ = return ()

verboseTrace :: RufousOptions -> String -> IO ()
verboseTrace opts msg = doIf verbose opts (putStrLn msg)

debugTrace :: RufousOptions -> String -> IO ()
debugTrace opts msg = doIf debug opts (putStrLn msg)
