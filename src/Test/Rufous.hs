module Test.Rufous(
   -- Main API
     runRufous
   , runRufousWithOptions
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
   , G.makeDUG

   -- Evaluator Stage
   , R.TimingDug
   , R.runDUG

   -- Select stage
   , Se.select

   -- TH Constructor
   , makeADTSpec
)
where

import Control.Exception

import Lens.Micro

import Test.Rufous.Options as Opt
   ( RufousOptions(..), DebugOptions(..), debugFlag, debugOpt)

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

debugArgs = 
   DebugOptions 
      { dumpDugs=False
      , dumpDir="./"
      , dumpPhaseTiming=True 
      , showNullTimes=True
      }
args = 
   RufousOptions 
      { signature=error "args :: no signature specified"
      , profiles=[]
      , dugs=[]
      , averageDugSize=10
      , numberOfTests=1
      , debug=False
      , debugOptions=debugArgs 
      }

runRufousOnDugs :: RufousOptions -> S.Signature -> [D.DUG a] -> IO ()
runRufousOnDugs opts s dugs = do
   let impls = s ^. S.nullImpl : s ^. S.implementations
   runDugs <- T.time ("RUN PHASE") $ sequence $ map (R.runDUG impls) dugs

   if (debugFlag dumpDugs opts) 
      then dumpTimingDugs2dot opts runDugs
      else return ()

   let normalisedDugs = map (R.normaliseDug s) runDugs

   timingDugs <- do
      if debugFlag showNullTimes opts
         then return runDugs
         else return normalisedDugs

   T.time ("SELECT PHASE") $ Se.select s timingDugs

runRufousOnProfiles :: RufousOptions -> S.Signature -> [P.Profile] -> IO ()
runRufousOnProfiles args s profiles = do
   dugs <- mapM (\p -> T.time "GENERATE PHASE" $ makeDUG s p (averageDugSize args)) profiles
   if (debugFlag dumpDugs args) 
      then dumpDugs2dot args dugs
      else return ()
   
   case args of
      RufousOptions _ _ [] _ _ _ _ -> runRufousOnDugs args s dugs
      RufousOptions _ _ ds _ _ _ _ -> runRufousOnDugs args s ds

runRufousWithOptions :: RufousOptions -> IO ()
runRufousWithOptions args = do
   T.reset
   let s = signature args
   profiles <- 
      if null (profiles args)
         then T.time "GENERATE PHASE (gen profiles)" $ mapM (const $ G.generateProfile s) [1..(numberOfTests args)]
         else return (profiles args)

   if debug args
      then mapM_ (putStrLn . P.prettyShowProfile) profiles
      else return ()

   runRufousOnProfiles args s profiles

   if (debugFlag dumpPhaseTiming args)
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
runRufous s = runRufousWithOptions args{signature=s}
