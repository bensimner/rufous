> {-# LANGUAGE FlexibleContexts #-}
> module Test.Rufous.Stats where

Collecting information for evaluation:

> import Control.Lens ((^.), (&))
> import Control.Applicative
> import Data.Time.Clock
> import System.Random (randomRIO)

> import System.Process

> import qualified Data.Aeson as A
> import qualified Data.ByteString.Lazy.Char8 as BL

> import qualified Data.Set as St
> import qualified Data.Map as M
> import qualified Test.Rufous.DUG as D
> import qualified Test.Rufous.Profile as P
> import qualified Test.Rufous.Signature as S
> import qualified Test.Rufous.Generate as G
> import qualified Test.Rufous.Extract as E
> import qualified Test.Rufous.Run as R

> import Debug.Trace

> diffProfiles :: P.Profile -> P.Profile -> Float
> diffProfiles p1 p2 = diffWeights * diffPersistents * diffMortality
>   where
>       diffWeights = diffMaps (p1 ^. P.operationWeights) (p2 ^. P.operationWeights)
>       diffPersistents = diffMaps (p1 ^. P.persistentApplicationWeights) (p2 ^. P.persistentApplicationWeights)
>       diffMortality = sqrDiff (p1 ^. P.mortality) (p2 ^. P.mortality)

> diffMaps :: M.Map String Float -> M.Map String Float -> Float
> diffMaps m1 m2 = sum $ sqrDiffMaps m1 m2

> sqrDiffMaps :: M.Map String Float -> M.Map String Float -> M.Map String Float
> sqrDiffMaps m1 m2 = fmap (uncurry sqrDiff) $ zipMaps m1 m2

> zipMaps :: M.Map String Float -> M.Map String Float -> M.Map String (Float, Float)
> zipMaps m1 m2 = M.fromList [(k, (m1 M.! k, m2 M.! k)) | k <- St.toList keys]
>   where
>       keys = M.keysSet m1 `St.union` M.keysSet m2

> sqrDiff :: Float -> Float -> Float
> sqrDiff x y = (x - y) ** 2

Evaluation 1
------------

To evaluate the performance/correctness of the generation algorithm -- how
well it can generate DUGs that conform to a specific profile, we generate
many profiles then generate DUGs from those profiles and their differences 
from the orgiginal profile

> generateProfile :: S.Signature -> IO P.Profile
> generateProfile s = do
>   mortality <- randomRIO (0.0, 1.0) 
>   weights <- generateMapWeights (s ^. S.operations & M.keys)
>   persistWeights <- generateMapWeights (s ^. S.operations & M.keys)
>   return $ P.Profile weights persistWeights mortality

> generateMapWeights :: [String] -> IO (M.Map String Float)
> generateMapWeights ss = do
>   ps <- generateProbabilities (length ss)
>   return $ generateFromPairs (ss `zip` ps)
>
> generateProbabilities :: Int -> IO [Float]
> generateProbabilities n = do
>       ps <- go n
>       let n = sum ps
>       return $ map (/ n) ps
>   where
>       go 0 = return []
>       go k = do
>           x <- randomRIO (0, 100)
>           xs <- go (k - 1)
>           return (x:xs)
>           
> generateFromPairs :: [(String, Float)] -> M.Map String Float
> generateFromPairs pairs = go pairs M.empty
>   where
>       go [] m = m
>       go ((s, f):ss) m = do
>           let m' = M.insert s f m
>           go ss m'


> experiment1 :: S.Signature -> IO ()
> experiment1 s = do
>   runs <- generateN s 5
>   BL.writeFile "data1.json" (A.encode runs)
>   putStrLn "[1] done"

> generateN :: S.Signature -> Int -> IO [(P.Profile, [(Int, Float)])]
> generateN s 0 = return []
> generateN s n = do
>   xs <- generateN s (n - 1)
>   x <- generatePair s
>   return $ x:xs

> generatePair :: S.Signature -> IO (P.Profile, [(Int, Float)])
> generatePair s = do
>   p <- generateProfile s
>   pairs <- generateOfProfile s p
>   return (p, pairs)

> generateOfProfile :: S.Signature -> P.Profile -> IO [(Int, Float)]
> generateOfProfile s p = do
>   let n = 10
>   let ns = map (*n) [1..(floor $ 500/(fromIntegral n))]
>   xs <- sequence $ map (generateOfSize s p) ns
>   return $ ns `zip` xs

> generateOfSize :: S.Signature -> P.Profile -> Int -> IO Float
> generateOfSize s p n = do
>   dug <- G.makeDUG s p n
>   let profile = D.extractProfile s dug
>   let diff = diffProfiles p profile
>   return diff

Experiment 1b
-------------

To dispute null hypothesis, need to know what a comparison between random profiles looks:
    - Generate a pofile
    - Generate another, different, profile.
    - Compare results

> experiment1b :: S.Signature -> IO ()
> experiment1b s = do
>   xs <- sequence $ replicate 100 (experiment1bStep s)
>   BL.writeFile "data1b.json" (A.encode xs)
>   print "[1B] Done."

> experiment1bStep :: S.Signature -> IO Float
> experiment1bStep s = do
>   p <- generateProfile s
>   p' <- generateProfile s
>   return $ diffProfiles p p'

Experiment 2
------------

This experiment is designed to test the performance, in general, of the DUG generation algorithm as it scales
It is very similiar to experiment 1 in this regard.

> experiment2 :: S.Signature -> IO ()
> experiment2 s = do
>   runs <- generateDUGs s 5000
>   BL.writeFile "data2.json" (A.encode runs)
>   putStrLn "[2] done"

> generateDUGs :: S.Signature -> Int -> IO [(Int, NominalDiffTime)]
> generateDUGs s n = sequence $ map (recordGenerateDUG s . (*100)) [1..(floor $ (fromIntegral n) / 100)]

> recordGenerateDUG :: S.Signature -> Int -> IO (Int, NominalDiffTime)
> recordGenerateDUG s sz = do
>   print ("[2]",sz)
>   p <- generateProfile s
>   (_, time) <- R.record (G.makeDUG s p sz)
>   return (sz, time)

Experiemnt 3
------------

To discover the effectiveness and overheads of the extraction algorithm

The experiment is straight-forward:
    - Generate a profile
    - Generate a large DUG from that profile
    - Run the DUG on the extracted null implementation
    - Retrieve the output DUG
    - Extract output DUG profile
    - Compare to original

> type Size = Int
> type Exp3DataPoint = (P.Profile, Size, Float)

> experiment3 :: S.Signature -> IO ()
> experiment3 s = do
>   let dataPoints = 1
>   diffs <- sequence $ replicate dataPoints (experiment3Step s)
>   BL.writeFile "data3.json" (A.encode diffs)

> experiment3Step :: S.Signature -> IO Exp3DataPoint
> experiment3Step s = do
>   p <- generateProfile s 
>   n <- randomRIO (1, 50)
>   print ("[3]", n)
>   d <- G.makeDUG s p n
>   (_, extractedDUG) <- E.extract s (R.runDUG (s ^. S.nullExtractorImpl) d)
>   putStrLn (D.pprintDUG extractedDUG)
>   let p' = D.extractProfile s extractedDUG
>   let diff = diffProfiles p p'
>   return (p, n, diff)

