module Test.Rufous.Internal.Evaluation.Results
   ( mergeResults
   )
where

import Control.Lens

import qualified Data.Map as M

import qualified Test.Rufous.Profile as P
import qualified Test.Rufous.DUG as D

import Test.Rufous.Internal.Evaluation.Types

mergeResults :: Result -> Result -> Result
mergeResults r1 r2 =
   Result
      { _resultDUG=(r1^.resultDUG)
      , _resultProfile=mergeProfiles (r1^.resultProfile) (r2^.resultProfile)
      , _resultOpCounts=mergeMaps (r1^.resultOpCounts) (r2^.resultOpCounts)
      , _resultTimes=mergeTimes (r1^.resultTimes) (r2^.resultTimes)
      }

mergeTimes :: TimingInfo -> TimingInfo -> TimingInfo
mergeTimes t1 t2 =
   TInfo
      { _nullTime=mergeFractionals (t1^.nullTime) (t2^.nullTime)
      , _times=mergeFMaps (t1^.times) (t2^.times)
      }

mergeProfiles :: P.Profile -> P.Profile -> P.Profile
mergeProfiles p1 p2 =
   P.Profile
      { P._operationWeights=mergeFMaps (p1^.P.operationWeights) (p2^.P.operationWeights)
      , P._persistentApplicationWeights=mergeFMaps (p1^.P.persistentApplicationWeights) (p2^.P.persistentApplicationWeights)
      , P._mortality=mergeFractionals (p1^.P.mortality) (p2^.P.mortality)
      }

mergeMaps :: Ord k => M.Map k Int -> M.Map k Int -> M.Map k Int
mergeMaps m1 m2 =
   M.unionWith mergeInts m1 m2

mergeFMaps :: (Ord k, Fractional a) => M.Map k a -> M.Map k a -> M.Map k a
mergeFMaps m1 m2 =
   M.unionWith mergeFractionals m1 m2

mergeFractionals :: Fractional a => a -> a -> a
mergeFractionals f1 f2 = (f1 + f2) / 2.0

mergeInts :: Int -> Int -> Int
mergeInts n1 n2 = (n1 + n2) `div` 2
