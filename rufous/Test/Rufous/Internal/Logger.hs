--

module Test.Rufous.Internal.Logger
    ( info
    , log
    , debug

    -- a version of Option.doIf
    -- but reads the RufousOptions locally
    , doIfIO

    -- a mini progress bar
    , updateProgress
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

import qualified Test.Rufous.Internal.Utils as U

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
traceLn s = mapM_ (hPutStrLn stderr) (map ("[ INFO] " ++) (lines s))

logLn :: String -> IO ()
logLn s = mapM_ (hPutStrLn stderr) (map ("[  LOG] " ++) (lines s))

debugLn :: String -> IO ()
debugLn s = mapM_ (hPutStrLn stderr) (map ("[DEBUG] " ++) (lines s))

{- Progress Bar -}
updateProgress :: Int -> Int -> IO ()
updateProgress i maxi = do
        hPutStr stderr $ "\r[" ++ replicate len '#' ++ replicate (20-len) ' ' ++ "] " ++ show percInt ++ "%"
        hFlush stderr
    where
        perc :: Float
        perc = fromIntegral i / fromIntegral maxi

        percInt :: Int
        percInt = round $ 100 * perc

        len :: Int
        len = round $ 20 * perc

endProgress :: IO ()
endProgress = hPutStrLn stderr ""