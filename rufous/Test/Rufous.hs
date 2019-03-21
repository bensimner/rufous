{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Test.Rufous
(
   -- Main API
     mainWith
   , Opt.RufousOptions(..)
   , Opt.DebugOptions(..)
   , Opt.args
   , Opt.debugArgs

   , guardFailed
   , shadowUndefined

   -- Datatypes
   , S.Signature
   , S.Implementation
   , D.DUG
   , P.Profile

   -- TH Constructor
   , TH.makeADTSignature
)
where

import Control.Exception
import Control.Lens

import qualified Test.Rufous.Options as Opt

import qualified Test.Rufous.DUG as D
import qualified Test.Rufous.Signature as S
import qualified Test.Rufous.Profile as P
import qualified Test.Rufous.Generate as G
import qualified Test.Rufous.Run as R
import qualified Test.Rufous.Select as Se
import qualified Test.Rufous.TH as TH
import qualified Test.Rufous.Aggregate as Agg

-- | Case for guard on a shadow operation failing
--
-- example:
-- instance ListADT ShadowList where
--    listhead (Shadow 0) = guardFailed
guardFailed :: a
guardFailed = throw R.GuardFailed

-- | Case for an operation on a shadow operation that is not defined over shadows
--
-- example:
-- instance ListADT ShadowList where
--    listhead (Shadow 1) = shadowUndefined
shadowUndefined :: a
shadowUndefined = throw R.NotImplemented


-- | Aggregate a bunch of results into something more manageable to output
rufousAggregate :: Opt.RufousOptions -> [R.Result] -> IO [Agg.AggregatedResult]
rufousAggregate opts rs =
   case Opt.aggregator opts of
      Opt.KMeans -> Agg.aggregateKMeans (Opt.kmeansOptions (Opt.aggregationOptions opts)) rs

-- | runs Rufous on a set of DUGs to get timing info for each of them
runRufousOnDugs :: Opt.RufousOptions -> S.Signature -> [D.DUG] -> IO ()
runRufousOnDugs opts s dugs = do
   let nul = s ^. S.nullImpl
   let impls = s ^. S.implementations
   results <- mapM (\d -> R.run s d nul impls) dugs
   agg <- rufousAggregate opts results
   Se.select s agg

-- | runs Rufous on a set of Profile by generating DUGs
runRufousOnProfiles :: Opt.RufousOptions -> S.Signature -> [P.Profile] -> IO ()
runRufousOnProfiles opts s profiles = do
   dugs <- mapM (\p -> G.generateDUG opts s p (Opt.averageDugSize opts)) profiles

   Opt.doIf Opt.debug opts $ do
      mapM_ (print . D.extractProfile s) dugs
      Opt.doIf (Opt.dumpDugs . Opt.debugOptions) opts $ mapM_ (\d -> D.printDUG ("output/" ++ d^.D.name) d) dugs

   case Opt.dugs opts of
      [] -> runRufousOnDugs opts s dugs
      ds -> runRufousOnDugs opts s ds

-- | Entrypoint to Rufous
--   With the default arguments this will run Rufous for some ADT,
--   generating Profiles and DUGs and print a table of results
--   with a generalised performance.
mainWith :: Opt.RufousOptions -> IO ()
mainWith opts = do
   let s = Opt.signature opts
   Opt.verboseTrace opts ("Found implementations: " ++ show (s^.S.implementations))

   ps <-
      if null (Opt.profiles opts)
         then sequence (replicate (Opt.numberOfTests opts) (S.randomProfile s))
         else return (Opt.profiles opts)

   Opt.debugTrace opts "With Profiles: "
   mapM_ (\p -> Opt.debugTrace opts ("\t" ++ show p)) ps

   runRufousOnProfiles opts s ps
