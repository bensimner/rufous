{-# LANGUAGE TemplateHaskell #-}
module Test.Rufous.Profile where

import Control.Lens
import qualified Data.Map as M
import Data.List (intercalate, sortOn)

-- | A 'Profile' contains a brief set of usage statistics for a datatype.
-- Specifically there are 3 important statistics in this datatype:
--  - The weights of proportion for each of the applications of the operations (the 'operationWeights')
--  - The weights of persistent applications of those operations (the 'persistentApplicationWeights')
--  - The weights of dead/mutated notes (the 'mortality', aka the relative depth of the 'DUG')
data Profile =
  Profile
      { _operationWeights :: M.Map String Float
      , _persistentApplicationWeights :: M.Map String Float
      , _mortality :: Float
      }
makeLenses ''Profile

instance Show Profile where
   show (Profile ws ps m) = "{weights={" ++ toStr ws ++ "}, pers={" ++ toStr ps ++ "}, mortality=" ++ (show m) ++ "}"
     where
         toStr m = intercalate ", " (map (\(k, a) -> k ++ ": " ++ show a) (M.toList m))

-- | Generate a simple 'Profile' from a list of operations
-- With a default mortality and persistent weights
fromList :: [String] -> Profile
fromList ops = Profile ws ps m
   where
      n = fromIntegral $ length ops
      ws = M.fromList $ map (\x -> (x, 1.0 / n)) ops
      ps = M.fromList $ map (\x -> (x, 0.5)) ops
      m = 0.5

-- | A float in [0, 1] whether a particular application of a given operation is likely to
-- be persistently applied given a particular profile.
pPersistent :: Profile -> String -> Float
pPersistent p s = (p ^. persistentApplicationWeights) M.! s

-- | A float in the range [0, 1] giving the probability of how likely a particular
-- operation is to appear in the 'DUG'.
pWeights :: Profile -> String -> Float
pWeights p s = (p ^. operationWeights) M.! s
