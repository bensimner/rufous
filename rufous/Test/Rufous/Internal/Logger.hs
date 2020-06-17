-- | Internal logging module
module Test.Rufous.Internal.Logger
    ( info
    , log
    , debug

    -- a version of Option.doIf
    -- but reads the RufousOptions locally
    , doIfIO

    -- a mini progress bar
    , initProgress
    , initProgressWithMsg
    , updateProgress
    , updateProgressMsg
    , endProgress

    -- | initial update of global IORef (see below)
    , initOptRef
   )
where

import Prelude hiding (log)

import System.IO
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.IORef as Ref

import Test.Rufous.Options hiding (debug)
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
info :: String -> IO ()
info s = do
   opts <- Ref.readIORef optionRef
   verboseTrace opts s

log :: String -> IO ()
log s = do
   opts <- Ref.readIORef optionRef
   logTrace opts s

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
verboseTrace :: RufousOptions -> String -> IO ()
verboseTrace opts msg = doIf verbose opts (traceLn msg)

logTrace :: RufousOptions -> String -> IO ()
logTrace opts msg = doIf ((>= 2) . logLevel) opts (logLn msg)

debugTrace :: RufousOptions -> String -> IO ()
debugTrace opts msg = doIf Opt.debug opts (debugLn msg)

traceLn :: String -> IO ()
traceLn s = last s `seq` mapM_ debugPutLn (map ("[ INFO] " ++) (lines s))

logLn :: String -> IO ()
logLn s = last s `seq` mapM_ debugPutLn (map ("[  LOG] " ++) (lines s))

debugLn :: String -> IO ()
debugLn s = last s `seq` mapM_ debugPutLn (map ("[DEBUG] " ++) (lines s))

debugPutLn :: String -> IO ()
debugPutLn s = do
    hPutStr stderr ("\r" ++ s)
    pg <- Ref.readIORef _progressBar
    case pg of
        Just (_, _, _, k) -> hPutStr stderr (replicate (k - length s) ' ')
        Nothing -> return ()
    hPutStr stderr "\n"
    refreshProgress

{- Progress Bar -}
ifShowProgress :: Opt.RufousOptions -> Bool
ifShowProgress = Opt.optFlag Opt.verbose Opt.showProgressBars

_progressBar :: Ref.IORef (Maybe (Int, Int, String, Int))
{-# NOINLINE _progressBar #-}
_progressBar = unsafePerformIO $ Ref.newIORef Nothing

initProgress :: Int -> IO ()
initProgress n = doIfIO ifShowProgress $ do
    Ref.writeIORef _progressBar (Just (0, n, "", 0))

initProgressWithMsg :: Int -> String -> IO ()
initProgressWithMsg n msg = doIfIO ifShowProgress $ do
    Ref.writeIORef _progressBar (Just (0, n, msg, 0))

refreshProgress :: IO ()
refreshProgress = doIfIO ifShowProgress $ do
    pg <- Ref.readIORef _progressBar
    case pg of
        Just (i, maxi, msg, k) -> do
            let perc :: Float; perc = fromIntegral i / fromIntegral maxi
            let percInt :: Int; percInt = round $ 100 * perc
            let len = round $ 20 * perc
            let progress = replicate len '#' ++ replicate (20-len) ' '
            let bar = "[" ++ progress ++ "] " ++ show percInt ++ "% " ++ msg
            hPutStr stderr $ "\r" ++ bar ++ replicate (k - length bar) ' '
            hFlush stderr
            Ref.writeIORef _progressBar (Just (i, maxi, msg, length bar))
            return ()
        Nothing -> return ()

updateProgress :: Int -> IO ()
updateProgress di = doIfIO ifShowProgress $ do
    Just (c, maxi, msg, k) <- Ref.readIORef _progressBar
    let i = c+di
    Ref.writeIORef _progressBar (Just (i, maxi, msg, k))
    refreshProgress

updateProgressMsg :: String -> IO ()
updateProgressMsg m = doIfIO ifShowProgress $ do
    Just (c, maxi, _, k) <- Ref.readIORef _progressBar
    Ref.writeIORef _progressBar (Just (c, maxi, m, k))
    refreshProgress

endProgress :: IO ()
endProgress = doIfIO ifShowProgress $ do
    Just (_, maxi, msg, k) <- Ref.readIORef _progressBar
    Ref.writeIORef _progressBar (Just (maxi, maxi, msg, k))
    refreshProgress
    Ref.writeIORef _progressBar Nothing
    hPutStrLn stderr ""