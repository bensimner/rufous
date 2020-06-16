{-# LANGUAGE ExistentialQuantification #-}
module Test.Rufous.Options
   ( RufousOptions(..)
   , DebugOptions(..)

   -- | Aggregation Options
   , AggregatorType(..)
   , AggregationOptions(..)

   -- | Aggreagtors
   , KMeansOptions(..)

   -- | Normalization of Rufous options
   , normalize

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
   , doIfElse

   -- | Options flags
   , verboseOnly
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
      , averageDugSizes :: [Int]
      , numberOfTests :: Int
      , numberOfRuns :: Int

      -- | Logging output
      -- Rufous has 4 output types:
      --  OUTPUT, TRACE, LOG, DEBUG
      --  logLevel defines which are visible:
      --  0 - OUTPUT
      --  1 - OUTPUT, TRACE
      --  2 - OUTPUT, TRACE, LOG
      --  3 - OUTPUT, TRACE, LOG, DEBUG
      --
      -- verbose=True sets logLevel>=1
      -- debug=True sets logLevel>=3
      , verbose :: Bool
      , debug :: Bool
      , logLevel :: Int

      -- | If logLevel >= 3 then Rufous can output
      -- additional information about the DUGs as it generates them
      , debugOptions :: DebugOptions

      -- | Unused
      , genOptions :: GenOptions

      -- | After running the DUGs and collecting timing info
      -- Rufous will aggregate the results into a single aggregated result
      -- e.g. using KMeans
      --
      -- aggregator selects the aggregation method
      -- aggregationOptions sets generic and aggregator-specific options
      , aggregationOptions :: AggregationOptions
      , aggregator :: AggregatorType
      }
   deriving (Show)

data DebugOptions =
   DebugOptions {
        dumpDir :: String
      , dumpDugs :: Bool
      , dumpPhaseTiming :: Bool
      , showNullTimes :: Bool
   }
   deriving (Show)

data GenOptions =
   GenOptions
   deriving (Show)

debugOpt :: (DebugOptions -> a) -> a -> RufousOptions -> a
debugOpt f x r = if debug r then f (debugOptions r) else x

debugFlag :: (DebugOptions -> Bool) -> RufousOptions -> Bool
debugFlag f r = debugOpt f False r

verboseOnly :: RufousOptions -> Bool
verboseOnly opts = verbose opts && not (debug opts)

{- Option-dependent Tracing Functions -}
doIf :: (RufousOptions -> Bool) -> RufousOptions -> IO () -> IO ()
doIf f opts a | f opts = a
doIf _ _ _ = return ()

doIfElse :: (RufousOptions -> Bool) -> RufousOptions -> IO () -> IO () -> IO ()
doIfElse f opts a _ | f opts = a
doIfElse _ _ _ b = b

normalize :: RufousOptions -> RufousOptions
normalize opt = opt{logLevel=newLogLevel}
   where
      newLogLevel =
         if debug opt
         then 3
         else
            if verbose opt
            then 1
            else 0


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
      , averageDugSizes=[10, 100, 1000, 5000]
      , numberOfTests=100
      , numberOfRuns=10
      , verbose=False
      , debug=False
      , logLevel=0
      , debugOptions=debugArgs
      , genOptions=genArgs
      , aggregator=KMeans
      , aggregationOptions=aggregationArgs
      }