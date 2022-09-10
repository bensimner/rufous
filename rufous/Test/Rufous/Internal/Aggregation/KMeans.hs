{-# LANGUAGE BangPatterns #-}

module Test.Rufous.Internal.Aggregation.KMeans
   ( aggregateKMeans
   )
where

import Control.Monad (when)
import Control.Lens

import System.Random (randomRIO)

import Data.List (sortOn, transpose)

import qualified Data.Map as M

import qualified Test.Rufous.Profile as P
import qualified Test.Rufous.Run as R

import Test.Rufous.Internal.Aggregation.Types
import qualified Test.Rufous.Internal.Logger as Log

aggregateKMeans :: KMeansOptions -> [R.Result] -> IO [AggregatedResult]
aggregateKMeans opts rs = do
      let n = numMeans opts
      Log.debug $ "Using KMeans"
      initClusters <- makeInitClusters n rs
      Log.debug $ "KMeans #init clusters=" ++ show (length initClusters)
      when (showKMeansSteps opts) $ do
         Log.debug $ "KMeans pointcloud: " ++ show (pointcloud rs)
         Log.debug $ "KMeans init clusters: " ++ show initClusters
      !clusters <- kmeans opts rs initClusters
      Log.debug $ "KMeans #clusters=" ++ show (length clusters)
      let groups = allocateGroups rs clusters
      let merged = map mergeGroup groups
      return merged

-- | Set of points from the results
pointcloud :: [R.Result] -> [[Float]]
pointcloud rs = map vec rs

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
generateInitCluster rs _ = do
      i <- randomRIO (0, length vecs - 1)
      return $ vecs !! i
   where vecs = map vec rs

kmeans :: KMeansOptions -> [R.Result] -> [[Float]] -> IO [[Float]]
kmeans opts rs initClusters = go initClusters
   where
      go c = do
         c' <- kmeansIter opts rs c
         Log.updateProgress 1
         if end c c' then
            return c'
         else
            go c'
      end c c' = all (uncurry isNear) (zip c c')

kmeansIter :: KMeansOptions -> [R.Result] -> [[Float]] -> IO [[Float]]
kmeansIter opts rs clusters = do
      when (showKMeansSteps opts) $ do
         Log.debug $ "KMeans Step: " ++ show clusters
         Log.debug $ "KMeans Groups: " ++ show groupVecs
         Log.debug $ "KMeans #clusters': " ++ show (length clusters')
      return clusters'
   where groups = allocateGroups rs clusters
         groupVecs = map (map vec) groups
         clusters' = map centre $ groupVecs

orderedElems :: M.Map String a -> [a]
orderedElems m = map snd (orderedPairs m)

orderedPairs :: M.Map String a -> [(String, a)]
orderedPairs m = sortOn fst (M.toList m)

vec :: R.Result -> [Float]
vec r = norm $ ws ++ ps ++ [m] ++ [s]
   where ws = orderedElems $ p ^. P.operationWeights
         ps = orderedElems $ p ^. P.persistentApplicationWeights
         m = p ^. P.mortality
         p = r ^. R.resultProfile
         s = log $ fromIntegral $ p ^. P.size

norm :: [Float] -> [Float]
norm v = map (/ s) v
   where s = sum v

centre :: [[Float]] -> [Float]
centre vecs = map (/ n) $ map sum (transpose vecs)
   where n = fromIntegral (length vecs)

isNear :: [Float] -> [Float] -> Bool
isNear v1 v2 =
   all (uncurry isFNear) (zip v1 v2)

isFNear :: Float -> Float -> Bool
isFNear f1 f2 =
      (isNaN f1 && isNaN f2)
   || (abs (f1 - f2) < 0.01)

allocateGroups :: [R.Result] -> [[Float]] -> [[R.Result]]
allocateGroups rs clusters = allocated
   where allocations = map (allocate clusters) rs
         pairs = zip rs allocations
         allocated = partition pairs clusters

partition :: [(R.Result, [Float])] -> [[Float]] -> [[R.Result]]
partition pairs clusters = partPairs ordered
   where tagged = [(r, i) | (r, alloc) <- pairs, (i, _) <- find alloc (ranged clusters)]
         ordered = sortOn snd tagged

find :: [Float] -> [(Int, [Float])] -> [(Int, [Float])]
find _ [] = []
find v ((i,c):_) | isNear v c = [(i,c)]
find v (_:cs) = find v cs

partPairs :: [(a, Int)] -> [[a]]
partPairs vs = go vs []
   where go [] x = [x]
         go [(a,_)] x = [a:x]
         go ((a,i):(b,j):xs) x =
            if i == j then
               go ((b,j):xs) (a:x)
            else
               (a:x) : go ((b,j):xs) []

-- | Pick the nearest centre to the result from the set of cluster centres
allocate :: [[Float]] -> R.Result -> [Float]
allocate fs r = snd m
   where v = vec r
         dists = map (dist v) fs
         pairs = zip dists fs
         m = minimum pairs

dist :: [Float] -> [Float] -> Float
dist x y = sum $ map (uncurry diff) (zip x y)
   where
      diff a b | isNaN a && isNaN b = 0
      diff a b | isNaN a || isNaN b = 1 -- since the vectors are normalised, 1 is the max distance they can have
      diff a b = (b - a) ** (2 :: Float)

ranged :: [a] -> [(Int, a)]
ranged xs = zip [0..] xs
