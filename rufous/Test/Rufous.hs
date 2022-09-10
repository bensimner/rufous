{-# LANGUAGE TemplateHaskell, FlexibleInstances, BangPatterns #-}
module Test.Rufous
(
   -- Main API
     mainWith
   , Opt.RufousOptions(..)
   , Opt.OutputOptions(..)
   , Opt.AggregationOptions(..)
   , Opt.KMeansOptions(..)
   , Opt.args
   , Opt.outputArgs
   , Opt.genArgs
   , Opt.aggregationArgs
   , Opt.kmeansArgs

   , guardFailed
   , shadowUndefined
   , extractorUndefined

   -- Datatypes
   , S.Signature
   , S.Implementation
   , D.DUG
   , P.Profile

   , S.addImpl
   , S.setNull
   , S.setShadow

   -- TH Constructor
   , TH.makeADTSignature
   , TH.makeExtractors

   -- DUG Extraction
   , E.extract
   , E.Extracted
)
where

import Control.Exception
import Control.Lens

import System.Random

import qualified Data.List as L

import qualified Test.Rufous.Options as Opt

import qualified Test.Rufous.DUG as D
import qualified Test.Rufous.Signature as S
import qualified Test.Rufous.Profile as P
import qualified Test.Rufous.Generate as G
import qualified Test.Rufous.Run as R
import qualified Test.Rufous.Select as Se
import qualified Test.Rufous.TH as TH
import qualified Test.Rufous.Aggregate as Agg
import qualified Test.Rufous.Extract as E
import qualified Test.Rufous.Exceptions as Exc

import qualified Test.Rufous.Internal.VerboseOutput as VB
import qualified Test.Rufous.Internal.Logger as Log
import qualified Test.Rufous.Internal.Utils as U

-- | Case for guard on a shadow operation failing
--
-- example:
-- > instance ListADT ShadowList where
-- >    listhead (Shadow 0) = guardFailed
guardFailed :: a
guardFailed = throw R.GuardFailed

-- | Case for an operation on a shadow operation that is not defined over shadows
--
-- example:
-- > instance ListADT ShadowList where
-- >    listhead (Shadow 1) = shadowUndefined
shadowUndefined :: a
shadowUndefined = throw R.NotImplemented


-- | For use as default implementation of the extractShadow function:
--
-- example:
-- > class ADT t where
-- >    ...
-- >    extractShadow :: t a -> ShadowType a
-- >    extractShadow = extractorUndefined
extractorUndefined :: a
extractorUndefined = throw R.NotImplemented

-- | Aggregate a bunch of results into something more manageable to output
rufousAggregate :: Opt.RufousOptions -> [R.Result] -> IO [Agg.AggregatedResult]
rufousAggregate opts rs =
   case Opt.aggregator opts of
      Opt.KMeans -> do
         Log.info $ "Using KMeans aggregation with " ++ show (Opt.aggregationOptions opts)
         Agg.aggregateKMeans (Opt.kmeansOptions (Opt.aggregationOptions opts)) rs

-- | Given a list of results from evaluating DUGs on many implementations
-- aggregate them and the print the table
aggregateResults :: Opt.RufousOptions -> S.Signature -> [R.Result] -> IO [Agg.AggregatedResult]
aggregateResults opts s results = do
   case R.splitResults results of
      Left (R.ResultFail f) -> do
         Log.out "** Failure to Evaluate DUG:"
         mapM_ (Log.out . ("   " ++)) (lines f)
         return []
      Right timingResults -> do
         Log.info $ "Got " ++ show (length timingResults) ++ " results"
         rufousAggregate opts timingResults

runRufousOnDug ::
      Opt.RufousOptions
   -> S.Signature
   -> S.Implementation
   -> [S.Implementation]
   -> D.DUG
   -> IO R.Result
runRufousOnDug opts s nul impls dug = do
   R.run s dug nul impls (Opt.numberOfRuns opts)

-- | Runs Rufous on a DUG
-- but does so strictly -- ensuring no remaining DUG
-- thunks are left behind
runRufousOnDug' ::
      Opt.RufousOptions
   -> S.Signature
   -> S.Implementation
   -> [S.Implementation]
   -> D.DUG
   -> IO R.Result
runRufousOnDug' opts s nul impls dug = do
   r <- R.run s dug nul impls (Opt.numberOfRuns opts)
   return $ R.forceResult r `seq` r

-- | runs Rufous on a set of DUGs to get timing info for each of them
-- rather than generating DUGs from profiles
runRufousOnDugs :: Opt.RufousOptions -> S.Signature -> [D.DUG] -> IO ()
runRufousOnDugs opts s dugs = do
   Opt.doIf Opt.verbose opts $ do
      mapM_ VB.logGeneratedDUG dugs

   nul <- U.unwrapJustIO (Exc.MissingNullImplementation (s^.S.signatureADTName)) (s ^. S.nullImpl)
   let impls = s ^. S.implementations
   Log.info $ "Found Null Implementation: " ++ show nul

   let n = length dugs

   -- for each DUG, for each observer, for each run, for each implementation (+ null impl) we have 1 evaluation.
   let numObservers = (1 + length impls) * Opt.numberOfRuns opts * sum [length $ D.observers d | d <- dugs]

   Log.info "Evaluating DUGs:"
   Log.debug $ "#observers=" ++ show numObservers
   Log.initProgressWithMsg numObservers ("Evaluated 0/" ++ show n ++ " DUGs")
   results <- sequence $ do
      (i, d) <- zip [1..] dugs
      return $ do
         Log.debug $ "Evaluating ... " ++ show i ++ "/" ++ show n
         r <- runRufousOnDug' opts s nul impls d
         Log.updateProgressMsg ("Evaluated " ++ show i ++ "/" ++ show n ++ " DUGs")
         return r
   Log.endProgress
   Log.info "Evaluated all DUGs"

   -- force all thunks for the results before trying to aggregate
   -- this ensures that all the DUGs are fully evaluated and GCd before continuing
   let () = L.foldl' (flip seq) () results

   Log.initUnboundedProgressWithMsg "Aggregating"
   agg <- aggregateResults opts s results
   Log.endProgress
   Se.select s agg

runRufousOnProfile :: Opt.RufousOptions -> S.Signature -> P.Profile -> Int -> Int -> IO R.Result
runRufousOnProfile opts s p i maxi = do
   nul <- U.unwrapJustIO (Exc.MissingNullImplementation (s ^. S.signatureADTName)) (s ^. S.nullImpl)
   let impls = s ^. S.implementations

   Log.debugIf (Opt.showDUGGeneration . Opt.outputOptions) "Generating DUG:"
   Log.debugIf (Opt.showDUGGeneration . Opt.outputOptions) $ " target profile=" ++ show p

   Log.updateProgressMsg $ "Generating DUG#" ++ show i ++ "/" ++ show maxi
   d <- G.generateDUG opts s p
   let d' = d & D.ginfo . _Just . D.idx .~ i

   Opt.doIf (Opt.dumpDUGs . Opt.outputOptions) opts $ do
      fname <- D.printDUGtoFile opts (Opt.dumpDir (Opt.outputOptions opts) ++ d'^.D.name) d'
      Log.info $ " produced " ++ fname

   Log.debugIf (Opt.showDUGGeneration . Opt.outputOptions) $ " extracted profile=" ++ show (D.extractProfile s d')
   Log.updateProgressMsg $ "Evaluating DUG#" ++ show i ++ "/" ++ show maxi
   runRufousOnDug' opts s nul impls d'

-- | runs Rufous on a set of Profile by generating DUGs
runRufousOnProfiles :: Opt.RufousOptions -> S.Signature -> [P.Profile] -> IO ()
runRufousOnProfiles opts s profiles = do
   let ndugs = length profiles
   let sizes = [p^.P.size | p <- profiles]
   let impls = s ^. S.implementations
   let noRuns = Opt.numberOfRuns opts * (1 + length impls)

   let barSize = sum sizes + noRuns * ndugs

   -- check if given an explicit set of DUGs to use,
   -- otherwise generate them from scratch
   case Opt.dugs opts of
      [] -> do
         Log.initProgressAll [Just barSize, Nothing] [2, 1] ("Generating DUG#0/" ++ show ndugs)
         -- generate and evaluate them immediately, one-by-one
         -- not remembering the DUG from last time.
         results <- U.psequence $ do
            (i, !p) <- (zip [1..] profiles :: [(Int,P.Profile)])
            return $ runRufousOnProfile opts s p i ndugs
         Log.updateProgressMsg "Aggregating Results"
         !agg <- aggregateResults opts s results
         Log.endProgress
         Se.select s agg
      ds -> runRufousOnDugs opts s ds

-- | Entrypoint to Rufous
--   With the default arguments this will run Rufous for some ADT,
--   generating Profiles and DUGs and print a table of results
--   with a generalised performance.
mainWith :: Opt.RufousOptions -> IO ()
mainWith options = do
   let opts = Opt.normalize options
   -- need to init the options global IORef
   -- so that all the verbose trace stuff below works.
   Log.initOptRef opts

   let s = Opt.signature opts
   Log.info $ "Found implementations: " ++ show (s^.S.implementations)

   seed <-
      case Opt.randomSeed opts of
         (-1) -> randomRIO (1,100000)
         i -> return i

   Log.info $ "Using randomSeed: " ++ show seed

   let stdgen = mkStdGen seed
   setStdGen stdgen

   let avgSizes = Opt.averageDugSizes opts
   ps <-
      if null (Opt.profiles opts)
         then sequence (replicate (Opt.numberOfTests opts) (S.randomProfile avgSizes s))
         else return (Opt.profiles opts)

   Log.info $ "Generated " ++ show (length ps) ++ " random profiles"
   runRufousOnProfiles opts s ps