{-# LANGUAGE ExistentialQuantification #-}
module Test.Rufous.Options
   ( RufousOptions(..)
   , LogOptions(..)

   -- | Aggregation Options
   , AggregatorType(..)
   , AggregationOptions(..)

   -- | Aggreagtors
   , KMeansOptions(..)

   -- | Normalization of Rufous options
   , normalize

   -- | Mutators over Options
   , optFlag
   , optValue

   -- | default option sets
   , logArgs
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

      -- | Rufous can also output additional information
      -- about the DUGs as it generates them
      -- e.g. graphviz for each DUG,  progress bars etc
      , logOptions :: LogOptions

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

data LogOptions =
   LogOptions {
        dumpDir :: String
      , dumpDugs :: Bool
      , dumpPhaseTiming :: Bool
      , showNullTimes :: Bool
      , showProgressBars :: Bool
   }
   deriving (Show)

data GenOptions =
   GenOptions
   deriving (Show)

optValue :: (RufousOptions -> Bool) -> (LogOptions -> a) -> a -> RufousOptions -> a
optValue f g x r = if f r then g (logOptions r) else x

optFlag :: (RufousOptions -> Bool) -> (LogOptions -> Bool) -> RufousOptions -> Bool
optFlag f g r = if f r then g (logOptions r) else False

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
normalize opt = opt{logLevel=newLogLevel, debug=enableDebug, verbose=enableVerbose}
   where
      newLogLevel =
         case logLevel opt of
            i | i == -1 ->
               if debug opt
               then 3
               else
                  if verbose opt
                  then 1
                  else 0
            i | i <= 3 ->
               logLevel opt
            _ -> error "Rufous: unsupported logLevel:  only 0 <= logLevel <= 3  supported."
      enableDebug = newLogLevel >= 3
      enableVerbose= newLogLevel >= 1

{- Default Options -}
genArgs :: GenOptions
genArgs = GenOptions

logArgs :: LogOptions
logArgs =
   LogOptions
      { dumpDugs=False
      , dumpDir="./"
      , dumpPhaseTiming=True
      , showNullTimes=True
      , showProgressBars=True
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
      , logLevel=(-1)
      , logOptions=logArgs
      , genOptions=genArgs
      , aggregator=KMeans
      , aggregationOptions=aggregationArgs
      }