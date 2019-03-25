{-# LANGUAGE TemplateHaskell #-}
module Test.Rufous.Profile where

import Control.Lens
import qualified Data.Map as M
import Data.List (intercalate)

-- | A 'Profile' contains a brief set of usage statistics for a datatype.
-- Specifically there are 3 important statistics in this datatype:
--  - The weights of proportion for each of the applications of the operations (the 'operationWeights')
--  - The weights of persistent applications of those operations (the 'persistentApplicationWeights')
--  - The weights of dead/mutated notes (the 'mortality', aka the relative depth of the 'DUG')
--  - The total number of versions of that datastructure that was created (aka the 'size' of the DUG)
data Profile =
  Profile
      { _operationWeights :: M.Map String Float
      , _persistentApplicationWeights :: M.Map String Float
      , _mortality :: Float
      , _size :: Int
      }
makeLenses ''Profile

instance Show Profile where
   show (Profile ws ps m s) = 
      ("{weights={" 
       ++ toStr ws
       ++ "}, pers={"
       ++ toStr ps 
       ++ "}, mortality="
       ++ (show m)
       ++ ", size="
       ++ (show s)
       ++ "}")
     where
         toStr d = intercalate ", " (map (\(k, a) -> k ++ ": " ++ show a) (M.toList d))

-- | A float in [0, 1] whether a particular application of a given operation is likely to
-- be persistently applied given a particular profile.
pPersistent :: Profile -> String -> Float
pPersistent p s = (p ^. persistentApplicationWeights) M.! s

-- | A float in the range [0, 1] giving the probability of how likely a particular
-- operation is to appear in the 'DUG'.
pWeights :: Profile -> String -> Float
pWeights p s = (p ^. operationWeights) M.! s
