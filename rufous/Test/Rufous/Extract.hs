module Test.Rufous.Extract where

import qualified Test.Rufous.DUG as D
import qualified Test.Rufous.Signature as S

import System.IO.Unsafe
import Control.Concurrent.MVar

-- | The type of a wrapped up ADT arg
type ExtractArg t x = S.Arg (WrappedADT t x) x Int Bool

-- | The type of an unwrapped ADT arg
type UnwrappedArg t x = S.Arg (t x) x Int Bool

type ExtractedDUG = D.DUG
data WrappedADT t x =
  WrappedADT
      { nodeId :: Int
      , nodeArgs :: [ExtractArg t x]
      , nodeOp :: String
      , value :: UnwrappedArg t x
      }
  deriving (Show)

-- | During extraction of a DUG the program must keep track of some state
-- This state simply tracks the DUG as a list-of-births
data ExtractorState t x =
   ExtractorState
      { births :: [WrappedADT t x]
      , currentIndex :: Int
      }

emptyExtractorState :: ExtractorState t x
emptyExtractorState = ExtractorState [] 0

state :: MVar (ExtractorState t x)
state = unsafePerformIO $ newEmptyMVar
{-# NOINLINE state #-}

unpickedWrapped :: S.Signature -> WrappedADT t x -> (S.Operation, 

createDUGfromBirths :: S.Signature -> [WrappedADT t x] -> ExtractedDUG
createDUGfromBirths s births' = go (D.emptyDUG "extracted") births'
   where
      go d [] = d
      go d (b:bs) =
         let (sop, dargs, dyn) = unpickWrapped s b
         let d' = D.pushNew sop dargs dyn d in
         go d' bs

extract :: S.Signature -> IO a -> IO (a, ExtractedDUG)
extract s a = do
   let st = emptyExtractorState
   putMVar state st
   v <- a
   st' <- takeMVar state
   let dug = createDUGfromBirths s (births st')
   return (v, dug)

_log_operation :: String -> [ExtractArg t x] -> t x -> WrappedADT t x
_log_operation opName args x = unsafePerformIO $ updateWrapper opName args (S.Version x)
{-# NOINLINE _log_operation #-}

_log_observer :: String -> [ExtractArg t x] -> x -> x
_log_observer opName args x = unsafePerformIO $ do
   _ <- updateWrapper opName args (S.NonVersion (S.VersionParam x)) -- TODO: this won't always be a version param
   return x
{-# NOINLINE _log_observer #-}

updateWrapper :: String -> [ExtractArg t x] -> UnwrappedArg t x -> IO (WrappedADT t x)
updateWrapper opName args v = do
   (ExtractorState bs i) <- takeMVar state
   let w = WrappedADT i args opName v
   let st' = ExtractorState (w:bs) (i+1)
   putMVar state st'
   return w

getVersion :: WrappedADT t x -> t x
getVersion w = case value w of
   S.Version x -> x
   _ -> error "getVersion :: expected version arg"
