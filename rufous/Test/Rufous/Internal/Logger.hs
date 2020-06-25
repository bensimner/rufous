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
    , initProgressAll
    , initUnboundedProgressWithMsg
    , updateProgress
    , updateProgressMsg
    , progressBarNextSegment
    , endProgress

    -- | initial update of global IORef (see below)
    , initOptRef
   )
where

import Prelude hiding (log)

import Data.Time.Clock

import Data.List

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
        Just prog -> hPutStr stdout (replicate (progLastLength prog - length s) ' ')
        Nothing -> return ()
    hPutStr stdout "\n"
    refreshProgress

debugPutLn :: String -> IO ()
debugPutLn s = do
    hPutStr stderr ("\r" ++ s)
    pg <- Ref.readIORef _progressBar
    case pg of
        Just prog -> hPutStr stderr (replicate (progLastLength prog - length s) ' ')
        Nothing -> return ()
    hPutStr stderr "\n"
    refreshProgress

{- Progress Bar -}
progressRefreshRate :: Rational
progressRefreshRate = 1/5

progressBarLength :: Int
progressBarLength = 40

ifShowProgress :: Opt.RufousOptions -> Bool
ifShowProgress = Opt.optFlag ((>= 1) . Opt.verbosity) Opt.showProgressBars

-- | a Progress bar is a collection of segments that fill in order
-- some segments may be unbounded and require
data Progress =
    Progress {
          progLastUpdate :: UTCTime
        , progCurrentMessage :: String
        , progPartitions :: Int
        , progPartitionWeights :: [Int]
        , progPartitionMax :: [Maybe Int]
        , progCurrentPartition :: Int
        , progCurrentProgress :: Int
        , progLastLength :: Int
    }
    deriving (Show)

_progressBar :: Ref.IORef (Maybe Progress)
{-# NOINLINE _progressBar #-}
_progressBar = unsafePerformIO $ Ref.newIORef Nothing

initProgress :: Int -> IO ()
initProgress n = doIfIO ifShowProgress $ do
    ctime <- getCurrentTime
    Ref.writeIORef _progressBar (Just $ Progress ctime "" 1 [1] [Just n] 0 0 0)

initProgressAll :: [Maybe Int] -> [Int] -> String -> IO ()
initProgressAll ns ws msg = doIfIO ifShowProgress $ do
    ctime <- getCurrentTime
    Ref.writeIORef _progressBar (Just $ Progress ctime msg (length ns) ws ns 0 0 0)

initProgressWithMsg :: Int -> String -> IO ()
initProgressWithMsg n msg = doIfIO ifShowProgress $ do
    ctime <- getCurrentTime
    Ref.writeIORef _progressBar (Just $ Progress ctime msg 1 [1] [Just n] 0 0 0)

initUnboundedProgressWithMsg :: String -> IO ()
initUnboundedProgressWithMsg msg = doIfIO ifShowProgress $ do
    ctime <- getCurrentTime
    Ref.writeIORef _progressBar (Just $ Progress ctime msg 1 [1] [Nothing] 0 0 0)

makeSegment :: Int -> Int -> Int -> Int -> Maybe Int -> String
makeSegment segmentLength currentSegment thisSegment progress Nothing | currentSegment == thisSegment = ljust (replicate len '*')
    where len = progress `mod` 3
          ljust s = s ++ replicate (segmentLength - length s) '.'

makeSegment segmentLength currentSegment thisSegment progress (Just maxProgress) | currentSegment == thisSegment  = ljust (replicate len '#')
    where ljust s = s ++ replicate (segmentLength - length s) '.'
          perc :: Float
          perc = fromIntegral progress / fromIntegral maxProgress
          len = round $ fromIntegral segmentLength * perc

makeSegment segmentLength currentSegment thisSegment _ _ | currentSegment < thisSegment = replicate segmentLength '.'
makeSegment segmentLength currentSegment thisSegment _ _ | currentSegment > thisSegment = replicate segmentLength '#'

makeProgressSegments :: Int -> Int -> [Int] -> [Maybe Int] -> String
makeProgressSegments currentSegment currentProgress weights maxProgresses = "[" ++ intercalate " " segments ++ "]"
    where
        segmentLength i = round $ (fromIntegral progressBarLength :: Float) * (fromIntegral (weights !! i) / fromIntegral (sum weights))
        mkSegment i s = makeSegment (segmentLength i) currentSegment i currentProgress s
        segments = map (\(i, s) -> mkSegment i s) (zip [0..] maxProgresses)

refreshProgress :: IO ()
refreshProgress = doIfIO ifShowProgress $ do
    pg <- Ref.readIORef _progressBar
    case pg of
        Just prog -> do
            let k = progLastLength prog
            let bar = makeProgressSegments (progCurrentPartition prog) (progCurrentProgress prog) (progPartitionWeights prog) (progPartitionMax prog) ++ " " ++ progCurrentMessage prog
            hPutStr stderr $ "\r" ++ bar ++ replicate (k - length bar) '#'
            hFlush stderr
            ctime <- getCurrentTime
            Ref.writeIORef _progressBar (Just (prog{progLastUpdate=ctime, progLastLength=length bar}))
            return ()
        Nothing -> return ()

nextSegment :: Progress -> Progress
nextSegment prog = prog{progCurrentProgress=0, progCurrentPartition=1+progCurrentPartition prog}

progressBarNextSegment :: IO ()
progressBarNextSegment = doIfIO ifShowProgress $ do
    Just prog <- Ref.readIORef _progressBar
    Ref.writeIORef _progressBar (Just $ nextSegment prog)

updateProgressBar :: Progress -> Int -> Progress
updateProgressBar p@(Progress _ _ _ _ parts current progress _) di =
    case (parts !! current) of
        Just i | progress+di >= i -> nextSegment p
        Just _ -> p{progCurrentProgress=progress + di}
        Nothing -> p{progCurrentProgress=progress + di}

updateProgress :: Int -> IO ()
updateProgress di = doIfIO ifShowProgress $ do
    Just prog <- Ref.readIORef _progressBar
    let oldtime = progLastUpdate prog
    ctime <- getCurrentTime
    Ref.writeIORef _progressBar (Just $ updateProgressBar prog di)

    if toRational (diffUTCTime ctime oldtime) > progressRefreshRate then
        refreshProgress
    else
        return ()

updateProgressMsg :: String -> IO ()
updateProgressMsg m = doIfIO ifShowProgress $ do
    Just prog <- Ref.readIORef _progressBar
    let oldtime = progLastUpdate prog
    ctime <- getCurrentTime
    Ref.writeIORef _progressBar (Just $ prog{progCurrentMessage=m})

    if toRational (diffUTCTime ctime oldtime) > progressRefreshRate then
        refreshProgress
    else
        return ()

endProgress :: IO ()
endProgress = doIfIO ifShowProgress $ do
    Just prog <- Ref.readIORef _progressBar
    Ref.writeIORef _progressBar (Just prog{progCurrentPartition=1 + progPartitions prog})
    refreshProgress
    Ref.writeIORef _progressBar Nothing
    hPutStrLn stderr ""