{-# LANGUAGE ExistentialQuantification #-}
module Test.Rufous.Options
   ( RufousOptions(..)
   , OutputOptions(..)
   , GenOptions(..)

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

import Prelude hiding (log)

import qualified Test.Rufous.Profile as P
import qualified Test.Rufous.Internal.Signature.SignatureType as S
import qualified Test.Rufous.Internal.DUG.Types as D
import Test.Rufous.Internal.Aggregation.Types

data RufousOptions =
   RufousOptions
      { signature :: S.Signature
      , profiles :: [P.Profile]
      , dugs :: [D.DUG]
      , averageDugSizes :: [Int]
      , numberOfTests :: Int
      , numberOfRuns :: Int
      , randomSeed :: Int

      -- | whether to strictly check for shadows etc
      , strict :: Bool

      -- | Logging output
      -- Rufous has 4 output types:
      --  NORMAL, INFO, VERBOSE, DEBUG
      --  verbosity defines which are visible:
      --  0 - NORMAL
      --  1 - NORMAL, INFO
      --  2 - NORMAL, INFO, VERBOSE
      --  3 - NORMAL, INFO, VERBOSE, DEBUG
      --
      -- info=True sets verbosity>=1
      -- verbose=True sets verbosity>=2
      -- debug=True sets verbosity>=3
      , verbosity :: Int
      , info :: Bool
      , verbose :: Bool
      , debug :: Bool

      -- | Rufous can also output additional information
      -- about the DUGs as it generates them
      -- e.g. graphviz for each DUG,  progress bars etc
      , outputOptions :: OutputOptions

      -- | Options for tweaking the generation algorithm
      -- e.g. when to give up on guards, whether to evaluate the shadow etc
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

data Verbosity =
     NORMAL
   | INFO
   | VERBOSE
   | DEBUG
   deriving (Ord, Enum, Eq, Show)

data OutputOptions =
   OutputOptions {
        dumpDir :: String
      , dumpDUGs :: Bool
      , dumpDUGDetail :: Int -- 0 = just shape of DUG, 1 = operations 2 = shadows etc
      , dumpPhaseTiming :: Bool
      , showNullTimes :: Bool
      , showProgressBars :: Bool
   }
   deriving (Show)

data GenOptions =
   GenOptions {
      -- | whether to evaluate the shadow during generation
      -- without this the generated DUG might have invalid applications (e.g. `head []`)
        genEvalShadow :: Bool

      -- | how many attempts to satisfy a guard before giving up on that node
      , genFailGuardTimeout :: Int
      }
   deriving (Show)

optValue :: (RufousOptions -> Bool) -> (OutputOptions -> a) -> a -> RufousOptions -> a
optValue f g x r = if f r then g (outputOptions r) else x

optFlag :: (RufousOptions -> Bool) -> (OutputOptions -> Bool) -> RufousOptions -> Bool
optFlag f g r = if f r then g (outputOptions r) else False

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
normalize opt = opt{verbosity=newLogLevel, info=enableInfo, debug=enableDebug, verbose=enableVerbose}
   where
      newLogLevel =
         case verbosity opt of
            i | i == -1 ->
               if debug opt
               then 3
               else
                  if verbose opt
                  then 2
                  else
                     if info opt
                        then 1
                        else 0
            i | i <= 3 ->
               verbosity opt
            _ -> error "Rufous: unsupported verbosity:  only 0 <= verbosity <= 3  supported."
      enableDebug = newLogLevel >= 3
      enableVerbose= newLogLevel >= 2
      enableInfo = newLogLevel >= 1

{- Default Options -}
genArgs :: GenOptions
genArgs =
   GenOptions
      { genEvalShadow = True
      , genFailGuardTimeout = 10
      }

logArgs :: OutputOptions
logArgs =
   OutputOptions
      { dumpDUGs=False
      , dumpDUGDetail=1
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
      , randomSeed=(-1)
      , strict=False
      , info=False
      , verbose=False
      , debug=False
      , verbosity=(-1)
      , outputOptions=logArgs
      , genOptions=genArgs
      , aggregator=KMeans
      , aggregationOptions=aggregationArgs
      }