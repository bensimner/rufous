module Test.Rufous.RndUtil where

import System.Random
import qualified Data.Map as M

-- Given a selection of items with probabilities
-- choose one of them.
chooseRandom :: [(a, Float)] -> IO a
chooseRandom ws = do
    r <- randomRIO (0.0, 1.0)
    return $ go ws r

    where
        go ((v, w) : ws) r =
            if r < w then
                v
            else
                go ws (r - w)
        go [] _ = error "chooseRandom :: weights did not sum to 1"

-- Given a map of operations choose one at random
chooseOperation :: M.Map String Float -> IO String
chooseOperation = chooseRandom . M.toList

chooseNonVersion :: IO Int
chooseNonVersion = randomRIO (0, 10)

chooseUniform :: [a] -> IO a
chooseUniform xs = chooseRandom weighted
   where
      weighted = zip xs (repeat pr)
      pr = 1 / (fromIntegral n)
      n = length xs

randomFlag :: Float -> IO Bool
randomFlag p = (p >) <$> randomRIO (0.0, 1.0) 

