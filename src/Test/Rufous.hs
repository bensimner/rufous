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

import System.FilePath
import System.Directory
import Control.Exception

import Lens.Micro

import Test.Rufous.DUG as D
import Test.Rufous.Signature as S
import Test.Rufous.Profile as P

import Test.Rufous.Extract as E
import Test.Rufous.Generate as G
import Test.Rufous.Run as R
import Test.Rufous.Select as Se

import Test.Rufous.TH as TH
import Test.Rufous.Stats as Stat
import Test.Rufous.Exceptions as Ex


data RufousOptions =
   RufousOptions
      { dumpDugs :: Bool
      , dumpDir :: String
      , avgDugSize :: Int
      , numberOfTests :: Int
      }
   deriving (Eq, Show)

defaultOptions = RufousOptions { dumpDugs=False, dumpDir="./dmp/", avgDugSize=10, numberOfTests=1 }

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
   dugs <- mapM (\p -> makeDUG s p (avgDugSize opts)) profiles
   if (dumpDugs opts) then
      printDugs opts dugs
   else return ()

   runRufousOnDugs opts s dugs

runRufousWithOptions :: RufousOptions -> S.Signature -> IO ()
runRufousWithOptions opts s = do
   profiles <- mapM (const $ generateProfile s) [1..(numberOfTests opts)]
   runRufousOnProfiles opts s profiles


guardFailed = throw Ex.GuardFailed
shadowUndefined = throw Ex.NotImplemented

printTimingDugs :: RufousOptions -> [R.TimingDug a] -> IO ()
printTimingDugs o [] = return ()
printTimingDugs o (d:dugs) = do 
      tryErr $ createDirectory (dumpDir o) 
      D.dug2dot' d (\n -> (n ^. D.node & snd & map snd & show)) (const "") fName
      printTimingDugs o dugs
   where name = maybe "dug" id (d ^. D.dugName)
         fName = (dumpDir o) </> name ++ "_timing"
   
printDugs :: RufousOptions -> [G.GenDUG] -> IO ()
printDugs o [] = return ()
printDugs o (d:dugs) = do 
      tryErr $ createDirectory (dumpDir o) 
      D.dug2dot d fName
      printDugs o dugs
   where name = maybe "dug" id (d ^. D.dugName)
         fName = (dumpDir o) </> name

tryErr :: IO () -> IO (Either IOError ())
tryErr = try

runRufous :: S.Signature -> IO ()
runRufous = runRufousWithOptions defaultOptions
