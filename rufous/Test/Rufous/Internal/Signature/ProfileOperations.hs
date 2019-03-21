module Test.Rufous.Internal.Signature.ProfileOperations where

import Control.Lens
import System.Random

import qualified Data.Map as M

import Test.Rufous.Internal.Signature.Types
import Test.Rufous.Profile

-- These are the Auburn-defined pmf and pof statistics.
-- pmf is the "persistent mutation factor" and pof is the "persistent observation factor".
-- They give a weight to how often the mutators/observers are persistently applied over the
-- entire DUG.
-- Here we _approximate_ the Auburn pmf/pof by taking the average of individual pf's for all operations
-- but this isn't precisely the same thing.
pmf :: Signature -> Profile -> Float
pmf s p = sum pmfs / (fromIntegral (length pmfs))
  where mutators = [k | (k, o) <- M.toList (s^.operations), Mutator == o^.opCategory]
        pmfs = [pPersistent p k | k <- mutators]

pof :: Signature -> Profile -> Float
pof s p = sum pofs / (fromIntegral (length pofs))
  where observers = [k | (k, o) <- M.toList (s^.operations), Observer == o^.opCategory]
        pofs = [pPersistent p k | k <- observers]

-- | Generate a random Profile for a given Signature
randomProfile :: Signature -> IO Profile
randomProfile s = do
      m <- randomMortality
      let ops = (M.elems (s^.operations))
      ps <- randomPersistents ops
      ws <- randomWeights ops
      return $ Profile ws ps m

randomOps :: [Operation] -> IO Float -> IO (M.Map String Float)
randomOps ops m = do
      pairs <- sequence $ map go ops
      return $ M.fromList pairs
   where go o = do
            p <- m
            return (o^.opName, p)

-- TODO: research+justify
randomMortality :: IO Float
randomMortality = randomListIO [0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.6, 0.8]

-- TODO: research+justify
randomPersistent :: IO Float
randomPersistent = randomListIO [0.001, 0.01, 0.05, 0.075, 0.1, 0.2, 0.5]

-- TODO: research+justify
randomWeight :: IO Float
randomWeight = randomRIO (0, 1)

randomListIO :: [a] -> IO a
randomListIO xs = do
   let n = length xs
   i <- randomRIO (0, n - 1)
   return $ xs !! i

randomPersistents :: [Operation] -> IO (M.Map String Float)
randomPersistents ops = randomOps ops randomPersistent

randomWeights :: [Operation] -> IO (M.Map String Float)
randomWeights ops = norm <$> randomOps ops randomWeight

norm :: M.Map String Float -> M.Map String Float
norm m = M.map (/ total) m
   where total = sum $ M.elems m
