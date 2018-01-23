module Test.Rufous.Random where

import System.Random

import qualified Data.Set as S
import qualified Data.Map as M

chooseWeighted :: [(a, Float)] -> IO a
chooseWeighted ws = do
   r <- randomRIO (0.0, 1.0)
   return $ go ws r

   where
      go ((x, w) : ws) r = 
         if r < w then
            x
         else
            go ws (r - w)
      go [] _ = error "chooseWeighted !! weights did not sum to 1"

chooseUniform :: S.Set a -> IO a
chooseUniform s = do
   r <- randomRIO (0, S.size s - 1)
   return $ S.elemAt r s

-- returns True with probability p
randomBool :: Float -> IO Bool
randomBool p = (p >) <$> randomRIO (0.0, 1.0)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]
