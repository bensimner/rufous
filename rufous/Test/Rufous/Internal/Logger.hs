--

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
traceLn s = last s `seq` mapM_ (hPutStrLn stderr) (map ("[ INFO] " ++) (lines s))

logLn :: String -> IO ()
logLn s = last s `seq` mapM_ (hPutStrLn stderr) (map ("[  LOG] " ++) (lines s))

debugLn :: String -> IO ()
debugLn s = last s `seq` mapM_ (hPutStrLn stderr) (map ("[DEBUG] " ++) (lines s))

{- Progress Bar -}
_progressBar :: Ref.IORef (Int, Int, String)
{-# NOINLINE _progressBar #-}
_progressBar = unsafePerformIO $ Ref.newIORef (0,0,"")

initProgress :: Int -> IO ()
initProgress n =
    Ref.writeIORef _progressBar (0, n, "")

initProgressWithMsg :: Int -> String -> IO ()
initProgressWithMsg n msg = do
    Ref.writeIORef _progressBar (0, n, msg)

refreshProgress :: IO ()
refreshProgress = do
    (i, maxi, msg) <- Ref.readIORef _progressBar
    let perc :: Float; perc = fromIntegral i / fromIntegral maxi
    let percInt :: Int; percInt = round $ 100 * perc
    let len = round $ 20 * perc
    hPutStr stderr $ "\r[" ++ replicate len '#' ++ replicate (20-len) ' ' ++ "] " ++ show percInt ++ "% " ++ msg
    hFlush stderr

updateProgress :: Int -> IO ()
updateProgress di = do
    (c, maxi, msg) <- Ref.readIORef _progressBar
    let i = c+di
    Ref.writeIORef _progressBar (i, maxi, msg)
    refreshProgress

updateProgressMsg :: String -> IO ()
updateProgressMsg m = do
    (c, maxi, _) <- Ref.readIORef _progressBar
    Ref.writeIORef _progressBar (c, maxi, m)
    refreshProgress

endProgress :: IO ()
endProgress = do
    (_, maxi, msg) <- Ref.readIORef _progressBar
    Ref.writeIORef _progressBar (maxi, maxi, msg)
    refreshProgress
    hPutStrLn stderr ""