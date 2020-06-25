-- | This module contains helpers for verbose output at various stages of Rufous
-- either with verbose=True or debug=True
module Test.Rufous.Internal.VerboseOutput where

import Control.Lens

import Data.List (intercalate)
import Data.Map (toList)
import Data.Maybe (fromJust)

import qualified Test.Rufous.Internal.Logger as Log

import qualified Test.Rufous.DUG as D
import qualified Test.Rufous.Run as R

logGeneratedDUG :: D.DUG -> IO ()
logGeneratedDUG d = do
        Log.log $ "DUG #" ++ show i ++ " (" ++ name ++ "):"
        Log.log $ " target profile: " ++ show profile
    where
        info = fromJust $ d ^. D.ginfo
        i = info ^. D.idx
        name = d^.D.name
        profile = info ^. D.targetProfile

logTimingResult :: Int -> R.Result -> IO ()
logTimingResult i r = do
        Log.log   $ "DUG #" ++ show i
        Log.log   $ "  ran " ++ show n ++ " times"
        Log.debug $ "  allTimes: " ++ show (map timingOut times)
        Log.log   $ "  avg time: " ++ timingOut (r^.R.resultAvgTimes)
    where
        times = r ^. R.resultAllTimings
        n = length times
        timingOut t =
            case t of
                R.DUGEvalFail _ -> " Evaluation Failed."
                R.DUGEvalTimes t' -> "[" ++ intercalate ", " [show impl ++ "=" ++ show time | (impl, time) <- toList (t'^.R.times)] ++ "]"