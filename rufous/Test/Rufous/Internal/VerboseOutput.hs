-- | This module contains helpers for verbose output at various stages of Rufous
-- either with verbose=True or debug=True
module Test.Rufous.Internal.VerboseOutput where

import Control.Lens

import Data.List (intercalate)
import Data.Map (toList)
import Data.Maybe (fromJust)

import qualified Test.Rufous.Options as Opt

import qualified Test.Rufous.DUG as D
import qualified Test.Rufous.Signature as S
import qualified Test.Rufous.Profile as P
import qualified Test.Rufous.Generate as G
import qualified Test.Rufous.Run as R
import qualified Test.Rufous.Select as Se
import qualified Test.Rufous.TH as TH
import qualified Test.Rufous.Aggregate as Agg
import qualified Test.Rufous.Extract as E


verbosePrintGeneratedDug :: D.DUG -> IO ()
verbosePrintGeneratedDug d =
        Opt.traceLn $ ("DUG #" ++ show i ++ " (" ++ name ++ ")")
    where i = fromJust $ d ^? D.ginfo . _Just . D.idx
          name = d^.D.name

debugPrintGeneratedDug :: D.DUG -> IO ()
debugPrintGeneratedDug d =
        mapM_ Opt.traceLn $ [
              "DUG #" ++ show i ++ " (" ++ name ++ "):"
            , " target profile: " ++ show profile
        ]
    where
        info = fromJust $ d ^. D.ginfo
        i = info ^. D.idx
        name = d^.D.name
        profile = info ^. D.targetProfile

verbosePrintTimingResults :: R.Result -> IO ()
verbosePrintTimingResults r =
        mapM_ Opt.traceLn $
            [ "DUG #" ++ show i ++ " (" ++ name ++ "):"
            , "  Target Profile:    " ++ show targetProfile
            , "  Generated Profile: " ++ show generatedProfile
            , timingOut
            ]
    where
        d = r^.R.resultDUG
        info = fromJust $ d ^. D.ginfo
        i = info ^. D.idx
        name = d ^. D.name
        targetProfile = info ^. D.targetProfile
        generatedProfile = r ^. R.resultProfile
        timingOut =
            case r^.R.resultTimes of
                Left _ -> " Evaluation Failed."
                Right t -> " Times: [" ++ intercalate ", " [show impl ++ "=" ++ show time | (impl, time) <- toList (t^.R.times)] ++ "]"

debugPrintTimingResults :: R.Result -> IO ()
debugPrintTimingResults r =
        mapM_ Opt.debugLn $
            [ "DUG #" ++ show i ++ " (" ++ name ++ "):"
            , "  Target Profile:    " ++ show targetProfile
            , "  Generated Profile: " ++ show generatedProfile
            ]
    where
        d = r^.R.resultDUG
        info = fromJust $ d ^. D.ginfo
        i = info ^. D.idx
        name = d ^. D.name
        targetProfile = info ^. D.targetProfile
        generatedProfile = r ^. R.resultProfile