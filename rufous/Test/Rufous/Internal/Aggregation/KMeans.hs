module Test.Rufous.Internal.Aggregation.KMeans 
   ( aggregateKMeans
   )
where

import Control.Lens

import System.Random (randomRIO)

import Data.List (sortOn, transpose)

import qualified Data.Map as M

import qualified Test.Rufous.Profile as P
import qualified Test.Rufous.Run as R

import Test.Rufous.Internal.Aggregation.Types

aggregateKMeans :: KMeansOptions -> [R.Result] -> IO [AggregatedResult]
aggregateKMeans opts rs = do
      let n = numMeans opts
      initClusters <- makeInitClusters n rs
      let clusters = kmeans n rs initClusters
      let groups = allocateGroups rs clusters
      let merged = map mergeGroup groups
      return merged


assert p s _ | not p = error s
assert _ _ k = k

mergeGroup :: [R.Result] -> AggregatedResult
mergeGroup rs = AggregatedResult rs r'
   where
      r' = foldl1 R.mergeResults rs

makeInitClusters :: Int -> [R.Result] -> IO [[Float]]
makeInitClusters nclusters rs =
      sequence $ replicate nclusters $ generateInitCluster rs diffs
   where
         vecs = map vec rs
         diffs = map diff (transpose vecs)
         diff vs = (minimum vs, maximum vs)

generateInitCluster :: [R.Result] -> [(Float, Float)] -> IO [Float]
generateInitCluster rs diffs = do
      i <- randomRIO (0, length vecs - 1)
      let v = vecs !! i
      -- mapM diff (zip v diffs)
      return v
   where vecs = map vec rs
         diff (f, (m, n)) = do
            d <- randomRIO (m, n)
            return $ f + d

kmeans :: Int -> [R.Result] -> [[Float]] -> [[Float]]
kmeans nclusters rs init = go init
   where
      go c =
        case kmeansIter rs c of
          c' | end c c' -> c
          c'            -> go c'
      end c c' = all (uncurry isNear) (zip c c')

kmeansIter :: [R.Result] -> [[Float]] -> [[Float]]
kmeansIter rs clusters = clusters'
   where groups = allocateGroups rs clusters
         groupVecs = map (map vec) groups
         clusters' = map centre $ groupVecs

orderedElems :: M.Map String a -> [a]
orderedElems m = map snd (orderedPairs m)

orderedKeys :: M.Map String a -> [String]
orderedKeys m = map fst (orderedPairs m)

orderedPairs :: M.Map String a -> [(String, a)]
orderedPairs m = sortOn fst (M.toList m)

vec :: R.Result -> [Float]
vec r = ws ++ ps ++ [m]
   where ws = orderedElems $ p ^. P.operationWeights
         ps = orderedElems $ p ^. P.persistentApplicationWeights
         m = p ^. P.mortality
         p = r ^. R.resultProfile

centre :: [[Float]] -> [Float]
centre vecs = map (/ n) $ map sum (transpose vecs)
   where n = fromIntegral (length vecs)

isNear v1 v2 =
   all (uncurry isFNear) (zip v1 v2)

isFNear f1 f2 =
      (isNaN f1 && isNaN f2)
   || (abs (f1 - f2) < 0.01)

allocateGroups :: [R.Result] -> [[Float]] -> [[R.Result]]
allocateGroups rs clusters = allocated
   where allocations = map (allocate clusters) rs
         pairs = zip rs allocations
         -- allocated = [[p | (p, alloc) <- pairs, isNear alloc cluster ] | cluster <- clusters]
         allocated = partition pairs clusters

partition :: [(R.Result, [Float])] -> [[Float]] -> [[R.Result]]
partition pairs clusters = partPairs ordered
   where tagged = [(p, i) | (p, alloc) <- pairs, (i, cluster) <- find alloc (ranged clusters)]
         ordered = sortOn snd tagged

find v [] = []
find v ((i,c):cs) | isNear v c = [(i,c)]
find v (_:cs) = find v cs

partPairs :: [(a, Int)] -> [[a]]
partPairs vs = go vs []
   where go [] x = [x]
         go [(a,i)] x = [a:x]
         go ((a,i):(b,j):xs) x =
            if i == j then
               go ((b,j):xs) (a:x)
            else
               (a:x) : go ((b,j):xs) []

allocate :: [[Float]] -> R.Result -> [Float]
allocate fs r = snd m
   where v = vec r
         dists = map (dist v) fs
         pairs = zip dists fs
         m = minimum pairs

dist :: [Float] -> [Float] -> Float
dist x y = sum $ map (\(a,b) -> (b - a)^2) (zip x y)

ranged :: [a] -> [(Int, a)]
ranged xs = zip [0..] xs
