module Test.Rufous(
   -- Main API
     runRufous
   , runRufousWithOptions
   , RufousOptions(..)
   , DebugOptions(..)
   , defaultOptions
   , defaultDebugOptions

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

import Test.Rufous.Options 
   ( RufousOptions(..), DebugOptions(..), debugFlag
   , debugOpt, defaultOptions, defaultDebugOptions)

import Test.Rufous.DUG as D
import Test.Rufous.Signature as S
import Test.Rufous.Profile as P

import Test.Rufous.Extract as E
import Test.Rufous.Generate as G
import Test.Rufous.Run as R
import Test.Rufous.Select as Se

import Test.Rufous.TH as TH
import Test.Rufous.Exceptions as Ex

import Test.Rufous.Internal.Timing as T

runRufousOnDugs :: RufousOptions -> S.Signature -> [D.DUG a] -> IO ()
runRufousOnDugs opts s dugs = do
   let impls = s ^. S.nullImpl : s ^. S.implementations
   runDugs <- T.time ("RUN PHASE") $ sequence $ map (R.runDUG impls) dugs

   if (debugFlag dumpDugs opts) 
      then dumpTimingDugs2dot opts runDugs
      else return ()

   let normalisedDugs = map (R.normaliseDug s) runDugs
   T.time ("SELECT PHASE") $ Se.select s runDugs

runRufousOnProfiles :: RufousOptions -> S.Signature -> [P.Profile] -> IO ()
runRufousOnProfiles opts s profiles = do
   dugs <- mapM (\p -> T.time "GENERATE PHASE" $ makeDUG s p (averageDugSize opts)) profiles
   if (debugFlag dumpDugs opts) 
      then dumpDugs2dot opts dugs
      else return ()

   runRufousOnDugs opts s dugs

runRufousWithOptions :: RufousOptions -> S.Signature -> IO ()
runRufousWithOptions opts s = do
   T.reset
   profiles <- T.time "GENERATE PHASE (gen profiles)" $ mapM (const $ G.generateProfile s) [1..(numberOfTests opts)]
   runRufousOnProfiles opts s profiles
   if (debugFlag dumpPhaseTiming opts)
      then do 
         c <- T.collect
         putStrLn "Phase timings:"
         mapM_ (\(name, t) -> putStrLn $ name ++ ": " ++ show t) c
      else return ()

guardFailed = throw Ex.GuardFailed
shadowUndefined = throw Ex.NotImplemented

infixr 3 </>
p </> p2 =
   case last p of
      '/' -> p ++ p2
      _ -> p ++ "/" ++ p2

dumpTimingDugs2dot :: RufousOptions -> [R.TimingDug a] -> IO ()
dumpTimingDugs2dot o [] = return ()
dumpTimingDugs2dot o (d:dugs) = do 
      D.dug2dot' d (\n -> (n ^. D.node & snd & map snd & show)) (const "") fName
      dumpTimingDugs2dot o dugs
   where name = maybe "dug" id (d ^. D.dugName)
         fName = (dumpDir (debugOptions o)) </> name ++ "_timing"
   
dumpDugs2dot :: RufousOptions -> [G.GenDUG] -> IO ()
dumpDugs2dot o [] = return ()
dumpDugs2dot o (d:dugs) = do 
      D.dug2dot d fName
      dumpDugs2dot o dugs
   where name = maybe "dug" id (d ^. D.dugName)
         fName = (dumpDir (debugOptions o)) </> name

runRufous :: S.Signature -> IO ()
runRufous = runRufousWithOptions defaultOptions
