{-# LANGUAGE TemplateHaskell, FlexibleInstances, BangPatterns #-}
module Test.Rufous
(
   -- Main API
     mainWith
   , Opt.RufousOptions(..)
   , Opt.OutputOptions(..)
   , Opt.args
   , Opt.logArgs

   , guardFailed
   , shadowUndefined
   , extractorUndefined

   -- Datatypes
   , S.Signature
   , S.Implementation
   , D.DUG
   , P.Profile

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

import qualified Test.Rufous.Internal.VerboseOutput as VB
import qualified Test.Rufous.Internal.Logger as Log

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

runRufousOnDug ::
      Opt.RufousOptions
   -> S.Signature
   -> S.Implementation
   -> [S.Implementation]
   -> D.DUG
   -> IO R.Result
runRufousOnDug opts s nul impls dug = do
   !r <- R.run s dug nul impls (Opt.numberOfRuns opts)
   return r

-- | runs Rufous on a set of DUGs to get timing info for each of them
runRufousOnDugs :: Opt.RufousOptions -> S.Signature -> [D.DUG] -> IO ()
runRufousOnDugs opts s dugs = do
   Opt.doIf Opt.verbose opts $ do
      mapM_ VB.logGeneratedDUG dugs

   let nul = s ^. S.nullImpl
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
         r <- runRufousOnDug opts s nul impls d
         Log.updateProgressMsg ("Evaluated " ++ show i ++ "/" ++ show n ++ " DUGs")
         return r
   Log.endProgress
   Log.info "Evaluated all DUGs"

   case R.splitResults results of
      Left (R.ResultFail f) -> do
         Log.out "** Failure to Evaluate DUG:"
         mapM_ (Log.out . ("   " ++)) (lines f)
      Right timingResults -> do
         Log.info $ "Got " ++ show (length timingResults) ++ " results"
         Opt.doIf Opt.verbose opts $ do
            mapM_ VB.logTimingResults timingResults
         agg <- rufousAggregate opts timingResults
         Se.select s agg

-- | runs Rufous on a set of Profile by generating DUGs
runRufousOnProfiles :: Opt.RufousOptions -> S.Signature -> [P.Profile] -> IO ()
runRufousOnProfiles opts s profiles = do
   let ndugs = length profiles
   let size = sum [p^.P.size | p <- profiles]

   Log.info "Generating DUGs:"
   Log.initProgressWithMsg size ("Generated 0/" ++ show ndugs ++ " DUGs")
   dugs <- sequence $ do
      (i, !p) <- (zip [1..] profiles :: [(Integer,P.Profile)])
      return $ do
         Log.debug  $ "Generating ... " ++ show i ++ "/" ++ show (length profiles)
         !g <- G.generateDUG opts s p
         Log.updateProgressMsg ("Generated " ++ show i ++ "/" ++ show ndugs ++ " DUGs")
         let g' = g & D.ginfo . _Just . D.idx .~ i
         return g'

   Log.endProgress
   Log.info $ "Generated " ++ show (length dugs) ++ " random DUGs from those profiles"

   Opt.doIf Opt.debug opts $ do
      Log.debug $ "These DUGs have the following extracted profiles:"
      mapM_ (print . D.extractProfile s) dugs

   Opt.doIf Opt.verbose opts $ do
      Opt.doIf (Opt.dumpDUGs . Opt.outputOptions) opts $ do
         mapM_ (\d -> do
            fname <- D.printDUGtoFile opts ("output/" ++ d^.D.name) d
            Log.info $ "Produced " ++ fname) dugs

   case Opt.dugs opts of
      [] -> runRufousOnDugs opts s dugs
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