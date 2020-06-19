-- | Internal logging module
module Test.Rufous.Internal.Logger
    ( out
    , info
    , log
    , debug

    -- a version of Option.doIf
    -- but reads the RufousOptions locally
    , doIfIO
    , ifShowProgress

    -- a mini progress bar
    , initProgress
    , initProgressWithMsg
    , initUnboundedProgressWithMsg
    , updateProgress
    , updateProgressMsg
    , endProgress

    -- | initial update of global IORef (see below)
    , initOptRef
   )
where

import Prelude hiding (log)

import Data.Time.Clock

import System.IO
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.IORef as Ref

import Test.Rufous.Options hiding (info, verbose, debug)
import qualified Test.Rufous.Options as Opt

{- Option state -}

-- | a global IO ref used *only* for debug/trace/log convenience functions!
optionRef :: Ref.IORef RufousOptions
{-# NOINLINE optionRef #-}
optionRef = unsafePerformIO $ Ref.newIORef args

-- | Write the new option ref
initOptRef :: RufousOptions -> IO ()
initOptRef opts = Ref.writeIORef optionRef opts

{- convinence wrappers around debug logging -}
out :: String -> IO ()
out s = do
    opts <- Ref.readIORef optionRef
    outTrace opts s

info :: String -> IO ()
info s = do
   opts <- Ref.readIORef optionRef
   infoTrace opts s

log :: String -> IO ()
log s = do
   opts <- Ref.readIORef optionRef
   verboseTrace opts s

debug :: String -> IO ()
debug s = do
   opts <- Ref.readIORef optionRef
   debugTrace opts s

{- IO wrappers -}
doIfIO :: (RufousOptions -> Bool) -> IO () -> IO ()
doIfIO f a = do
    opts <- Ref.readIORef optionRef
    doIf f opts a

{- Internal Wrappers -}
outTrace :: RufousOptions -> String -> IO ()
outTrace opts msg = doIf ((>= 0) . verbosity) opts (outLn msg)

infoTrace :: RufousOptions -> String -> IO ()
infoTrace opts msg = doIf ((>= 1) . verbosity) opts (traceLn msg)

verboseTrace :: RufousOptions -> String -> IO ()
verboseTrace opts msg = doIf ((>= 2) . verbosity) opts (logLn msg)

debugTrace :: RufousOptions -> String -> IO ()
debugTrace opts msg = doIf ((>= 3) . verbosity) opts (debugLn msg)

outLn :: String -> IO ()
outLn s = last s `seq` mapM_ outPutLn (lines s)

traceLn :: String -> IO ()
traceLn s = last s `seq` mapM_ debugPutLn (map ("[ INFO] " ++) (lines s))

logLn :: String -> IO ()
logLn s = last s `seq` mapM_ debugPutLn (map ("[  LOG] " ++) (lines s))

debugLn :: String -> IO ()
debugLn s = last s `seq` mapM_ debugPutLn (map ("[DEBUG] " ++) (lines s))

outPutLn :: String -> IO ()
outPutLn s = do
    hPutStr stdout ("\r" ++ s)
    pg <- Ref.readIORef _progressBar
    case pg of
        Just (_, _, _, _, k) -> hPutStr stdout (replicate (k - length s) ' ')
        Nothing -> return ()
    hPutStr stdout "\n"
    refreshProgress

debugPutLn :: String -> IO ()
debugPutLn s = do
    hPutStr stderr ("\r" ++ s)
    pg <- Ref.readIORef _progressBar
    case pg of
        Just (_, _, _, _, k) -> hPutStr stderr (replicate (k - length s) ' ')
        Nothing -> return ()
    hPutStr stderr "\n"
    refreshProgress

{- Progress Bar -}
progressRefreshRate :: Rational
progressRefreshRate = 1/5

ifShowProgress :: Opt.RufousOptions -> Bool
ifShowProgress = Opt.optFlag ((>= 1) . Opt.verbosity) Opt.showProgressBars

_progressBar :: Ref.IORef (Maybe (UTCTime, Int, Maybe Int, String, Int))
{-# NOINLINE _progressBar #-}
_progressBar = unsafePerformIO $ Ref.newIORef Nothing

initProgress :: Int -> IO ()
initProgress n = doIfIO ifShowProgress $ do
    ctime <- getCurrentTime
    Ref.writeIORef _progressBar (Just (ctime, 0, Just n, "", 0))

initProgressWithMsg :: Int -> String -> IO ()
initProgressWithMsg n msg = doIfIO ifShowProgress $ do
    ctime <- getCurrentTime
    Ref.writeIORef _progressBar (Just (ctime, 0, Just n, msg, 0))

initUnboundedProgressWithMsg :: String -> IO ()
initUnboundedProgressWithMsg msg = doIfIO ifShowProgress $ do
    ctime <- getCurrentTime
    Ref.writeIORef _progressBar (Just (ctime, 0, Nothing, msg, 0))

refreshProgress :: IO ()
refreshProgress = doIfIO ifShowProgress $ do
    pg <- Ref.readIORef _progressBar
    case pg of
        Just (_, i, maybe_maxi, msg, k) -> do
            let bar =
                 (case maybe_maxi of
                    Just maxi ->
                        let perc :: Float; perc = fromIntegral i / fromIntegral maxi in
                        let percInt :: Int; percInt = round $ 100 * perc in
                        let len = round $ 20 * perc in
                        let progress = replicate len '#' ++ replicate (20-len) ' ' in
                        "[" ++ progress ++ "] " ++ show percInt ++ "% " ++ msg
                    Nothing ->
                        let flick = flickerAnim i in
                        "[" ++ flick ++ "] " ++ show i ++ " " ++ msg)
            hPutStr stderr $ "\r" ++ bar ++ replicate (k - length bar) ' '
            hFlush stderr
            ctime <- getCurrentTime
            Ref.writeIORef _progressBar (Just (ctime, i, maybe_maxi, msg, length bar))
            return ()
        Nothing -> return ()

flickerAnim :: Int -> String
flickerAnim i | i `mod` 3 == 0 = "/"
flickerAnim i | i `mod` 3 == 1 = "|"
flickerAnim i | i `mod` 3 == 2 = "\\"
flickerAnim _ = error "unreachable"

updateProgress :: Int -> IO ()
updateProgress di = doIfIO ifShowProgress $ do
    Just (oldtime, c, maxi, msg, k) <- Ref.readIORef _progressBar
    ctime <- getCurrentTime
    let i = c+di
    Ref.writeIORef _progressBar (Just (oldtime, i, maxi, msg, k))

    if toRational (diffUTCTime ctime oldtime) > progressRefreshRate then
        refreshProgress
    else
        return ()

updateProgressMsg :: String -> IO ()
updateProgressMsg m = doIfIO ifShowProgress $ do
    Just (oldtime, c, maxi, _, k) <- Ref.readIORef _progressBar
    ctime <- getCurrentTime

    Ref.writeIORef _progressBar (Just (oldtime, c, maxi, m, k))

    if toRational (diffUTCTime ctime oldtime) > progressRefreshRate then
        refreshProgress
    else
        return ()

endProgress :: IO ()
endProgress = doIfIO ifShowProgress $ do
    Just (t, i, maxi, msg, k) <- Ref.readIORef _progressBar
    case maxi of
        Just m -> Ref.writeIORef _progressBar (Just (t, m, Just m, msg, k))
        Nothing -> Ref.writeIORef _progressBar (Just (t, i, Nothing, msg, k))
    refreshProgress
    Ref.writeIORef _progressBar Nothing
    hPutStrLn stderr ""