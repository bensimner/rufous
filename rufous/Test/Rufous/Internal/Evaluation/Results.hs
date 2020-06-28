{-# LANGUAGE BangPatterns #-}

module Test.Rufous.Internal.Evaluation.Results
   ( mergeResults
   , mergeDUGTimeInfos
   , splitResultFailures
   , splitResults
   , forceResult
   )
where

import Control.Lens

import qualified Data.Map as M

import qualified Test.Rufous.Profile as P

import Test.Rufous.Internal.Evaluation.Types

mergeResults :: Result -> Result -> Result
mergeResults r1 r2 =
   Result
      { _resultProfile=mergeProfiles (r1^.resultProfile) (r2^.resultProfile)
      , _resultOpCounts=mergeMaps (r1^.resultOpCounts) (r2^.resultOpCounts)
      , _resultAllTimings=(r1^.resultAllTimings) ++ (r2^.resultAllTimings)
      , _resultAvgTimes=mergeDUGTimeInfos (r1^.resultAvgTimes) (r2^.resultAvgTimes)
      }

mergeDUGTimeInfos :: DUGTimeInfo -> DUGTimeInfo -> DUGTimeInfo
mergeDUGTimeInfos t1 t2 =
   case (t1, t2) of
      (DUGEvalTimes r1, DUGEvalTimes r2) -> DUGEvalTimes $ mergeTimes r1 r2
      (DUGEvalTimes _, f) -> f
      (f, _) -> f

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
      , P._size=mergeInts (p1^.P.size) (p2^.P.size)
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


-- | Given a list of possible results extract either the first failure
-- if it exists, or the list of successes if none failed.
splitResultFailures :: [Either ResultFailure a] -> Either ResultFailure [a]
splitResultFailures [] = error "Rufous: internal: unreachable: could not split results failures. got []."
splitResultFailures [Left f] = Left f
splitResultFailures [Right r] = Right [r]
splitResultFailures (Left f : _) = Left f
splitResultFailures (Right t : rs) =
   case splitResultFailures rs of
      Left f -> Left f
      Right xs -> Right (t : xs)

-- | Given a list of possible results extract either the first failure
-- if it exists, or the list of successes if none failed.
splitResults :: [Result] -> Either ResultFailure [Result]
splitResults [] = Left $ ResultFail "Cannot split Results: No Results?"
splitResults [r] =
   case r^.resultAvgTimes of
      DUGEvalFail f -> Left f
      _ -> Right [r]
splitResults (r:rs) =
   case r^.resultAvgTimes of
      DUGEvalFail f -> Left f
      _ ->
         case splitResults rs of
            Left f -> Left f
            Right xs -> Right (r:xs)

-- | Forcing evaluation of this to WHNF fully evaluates
-- all of the Result
forceResult :: Result -> ()
forceResult (Result p m t rs) =
      p `seq` forcedOpCounts `seq` (forceDUGTimes t) `seq` (map forceDUGTimes rs) `seq` ()
   where forcedOpCounts = forceMap m
         forceMap m' = M.foldl' (\b v -> v `seq` b) () m'
         forceDUGTimes (DUGEvalFail _) = ()
         forceDUGTimes (DUGEvalTimes t) = forceTinfo t
         forceTinfo tinfo = tinfo^.nullTime `seq` (sum $ M.elems $ tinfo^.times) `seq` ()