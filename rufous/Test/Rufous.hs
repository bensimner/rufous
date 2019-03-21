{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Test.Rufous
{-
(
   -- Main API
     runRufous
   , mainWith
   , RufousOptions(..)
   , DebugOptions(..)
   , args
   , debugArgs

   , guardFailed
   , shadowUndefined

   -- Datatypes
   , S.Signature
   , S.Implementation
   , D.DUG
   , P.Profile


   -- Extractor Stage
   , E.extract

   -- Generator Stage
   , G.generateDUG

   -- Evaluator Stage
   , R.runDUG

   -- Select stage
   , Se.select

   -- TH Constructor
   , TH.makeADTSignature
)
-}
where

import Control.Exception
import Control.Lens

import Test.Rufous.Options (args)
import qualified Test.Rufous.Options as Opt

import qualified Test.Rufous.DUG as D
import qualified Test.Rufous.Signature as S
import qualified Test.Rufous.Profile as P
import qualified Test.Rufous.Generate as G
import qualified Test.Rufous.Run as R
import qualified Test.Rufous.Select as Se
import qualified Test.Rufous.TH as TH
--import Test.Rufous.Exceptions as Ex


import qualified Test.Rufous.Aggregate as Agg

{- For Testing Purposes ... -}
class ListADT t where
   listcons :: a -> t a -> t a
   listempty :: t a
   listhead :: t a -> a

instance ListADT [] where
   listcons = (:)
   listempty = []
   listhead = head

newtype ShadowList t = S Int
instance ListADT ShadowList where
   listcons _ (S i) = S (i + 1)
   listempty = S 0
   listhead (S 0) = throw R.GuardFailed
   listhead (S _) = throw R.NotImplemented

data FakeList a = EF | F a (FakeList a)
instance ListADT FakeList where
   listcons y f = F y f
   listempty = EF
   listhead EF = undefined
   listhead (F x _) = x

TH.makeADTSignature ''ListADT


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

   if Opt.debug opts
      then do
         mapM_ (print . D.extractProfile s) dugs
         mapM_ (\d -> D.printDUG ("output/" ++ d^.D.name) d) dugs
      else return ()

   case opts of
      Opt.RufousOptions _ _ [] _ _ _ _ _ _ -> runRufousOnDugs opts s dugs
      Opt.RufousOptions _ _ ds _ _ _ _ _ _ -> runRufousOnDugs opts s ds

-- | Entrypoint to Rufous
--   With the default arguments this will run Rufous for some ADT,
--   generating Profiles and DUGs and print a table of results
--   with a generalised performance.
mainWith :: Opt.RufousOptions -> IO ()
mainWith opts = do
   let s = Opt.signature opts
   print $ s ^. S.implementations
   ps <-
      if null (Opt.profiles opts)
         then sequence (replicate (Opt.numberOfTests opts) (S.randomProfile s))
         else return (Opt.profiles opts)

   if Opt.debug opts
      then mapM_ print ps
      else return ()

   runRufousOnProfiles opts s ps

main :: IO ()
main = do
   mainWith args{Opt.signature=_ListADT, Opt.debug=True, Opt.averageDugSize=500}

main' :: IO ()
main' = do
   mainWith args{Opt.signature=_ListADT, Opt.debug=True, Opt.averageDugSize=50, Opt.numberOfTests=50}
