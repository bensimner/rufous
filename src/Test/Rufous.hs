module Test.Rufous(
   -- Main API
     runRufous
   , runRufousWithOptions
   , RufousOptions(..)
   , defaultOptions

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
   , G.makeDUG

   -- Evaluator Stage
   , R.TimingDug
   , R.runDUG

   -- Select stage
   , Se.select

   -- TH Constructor
   , makeRufousSpec
)
where

import Control.Exception

import Lens.Micro

import Test.Rufous.Options (RufousOptions(..), defaultOptions)

import Test.Rufous.DUG as D
import Test.Rufous.Signature as S
import Test.Rufous.Profile as P

import Test.Rufous.Extract as E
import Test.Rufous.Generate as G
import Test.Rufous.Run as R
import Test.Rufous.Select as Se

import Test.Rufous.TH as TH
import Test.Rufous.Exceptions as Ex

runRufousOnDugs :: RufousOptions -> S.Signature -> [D.DUG a] -> IO ()
runRufousOnDugs opts s dugs = do
   let impls = s ^. S.nullImpl : s ^. S.implementations
   runDugs <- sequence $ map (R.runDUG impls) dugs

   if (dumpDugs opts) then
      printTimingDugs opts runDugs
   else return ()

   let normalisedDugs = map (R.normaliseDug s) runDugs
   Se.select s normalisedDugs

runRufousOnProfiles :: RufousOptions -> S.Signature -> [P.Profile] -> IO ()
runRufousOnProfiles opts s profiles = do
   dugs <- mapM (\p -> makeDUG s p (averageDugSize opts)) profiles
   if (dumpDugs opts) then
      printDugs opts dugs
   else return ()

   runRufousOnDugs opts s dugs

runRufousWithOptions :: RufousOptions -> S.Signature -> IO ()
runRufousWithOptions opts s = do
   profiles <- mapM (const $ G.generateProfile s) [1..(numberOfTests opts)]
   runRufousOnProfiles opts s profiles


guardFailed = throw Ex.GuardFailed
shadowUndefined = throw Ex.NotImplemented

infixr 3 </>
p </> p2 =
   case last p of
      '/' -> p ++ p2
      _ -> p ++ "/" ++ p2

printTimingDugs :: RufousOptions -> [R.TimingDug a] -> IO ()
printTimingDugs o [] = return ()
printTimingDugs o (d:dugs) = do 
      D.dug2dot' d (\n -> (n ^. D.node & snd & map snd & show)) (const "") fName
      printTimingDugs o dugs
   where name = maybe "dug" id (d ^. D.dugName)
         fName = (dumpDir o) </> name ++ "_timing"
   
printDugs :: RufousOptions -> [G.GenDUG] -> IO ()
printDugs o [] = return ()
printDugs o (d:dugs) = do 
      D.dug2dot d fName
      printDugs o dugs
   where name = maybe "dug" id (d ^. D.dugName)
         fName = (dumpDir o) </> name

runRufous :: S.Signature -> IO ()
runRufous = runRufousWithOptions defaultOptions
