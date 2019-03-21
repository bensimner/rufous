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
import Debug.Trace
import System.Random

import qualified Data.Map as M

import Test.Rufous.Options as Opt

import Test.Rufous.DUG as D hiding (args)
import Test.Rufous.Signature as S
import Test.Rufous.Profile as P

import Test.Rufous.Extract as E
import Test.Rufous.Generate as G
import Test.Rufous.Run as R
import Test.Rufous.Select as Se

import Test.Rufous.TH as TH
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
   listhead (S 0) = throw GuardFailed
   listhead (S i) = throw NotImplemented

data FakeList a = EF | F a (FakeList a)
instance ListADT FakeList where
   listcons y f = F y f
   listempty = EF
   listhead EF = undefined
   listhead (F x _) = x

TH.makeADTSignature ''ListADT


-- | Aggregate a bunch of results into something more manageable to output
rufousAggregate :: RufousOptions -> [R.Result] -> IO [Agg.AggregatedResult]
rufousAggregate opts rs =
   case aggregator opts of
      Opt.KMeans -> Agg.aggregateKMeans (Opt.kmeansOptions (aggregationOptions opts)) rs

-- | runs Rufous on a set of DUGs to get timing info for each of them
runRufousOnDugs :: RufousOptions -> S.Signature -> [D.DUG] -> IO ()
runRufousOnDugs opts s dugs = do
   let nul = s ^. S.nullImpl
   let impls = s ^. S.implementations
   results <- mapM (\d -> R.run s d nul impls) dugs
   agg <- rufousAggregate opts results
   Se.select s agg

-- | runs Rufous on a set of Profile by generating DUGs
runRufousOnProfiles :: RufousOptions -> S.Signature -> [P.Profile] -> IO ()
runRufousOnProfiles args s profiles = do
   dugs <- mapM (\p -> G.generateDUG args s p (averageDugSize args)) profiles

   if debug args
      then do
         mapM_ (print . D.extractProfile s) dugs
         mapM_ (\d -> D.printDUG ("output/" ++ d^.name) d) dugs
      else return ()

   case args of
      RufousOptions _ _ [] _ _ _ _ _ _ -> runRufousOnDugs args s dugs
      RufousOptions _ _ ds _ _ _ _ _ _ -> runRufousOnDugs args s ds

-- | Entrypoint to Rufous
--   With the default arguments this will run Rufous for some ADT,
--   generating Profiles and DUGs and print a table of results
--   with a generalised performance.
mainWith :: RufousOptions -> IO ()
mainWith args = do
   let s = signature args
   print $ s ^. S.implementations
   profiles <-
      if null (profiles args)
         then sequence (replicate (numberOfTests args) (S.randomProfile s))
         else return (profiles args)

   if debug args
      then mapM_ print profiles
      else return ()

   runRufousOnProfiles args s profiles

main = do
   mainWith args{signature=_ListADT, debug=True, averageDugSize=500}
