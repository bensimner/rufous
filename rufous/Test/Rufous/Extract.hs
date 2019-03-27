{-# LANGUAGE Rank2Types, BangPatterns #-}
module Test.Rufous.Extract where


import Control.Lens

import Unsafe.Coerce
import System.IO.Unsafe
import Control.Concurrent.MVar

import Data.Dynamic (Dynamic)

import qualified Data.Map as M

import qualified Test.Rufous.DUG as D
import qualified Test.Rufous.Signature as S

import Data.List (intercalate)

-- | The type of a wrapped up ADT arg
type ExtractArg t x = S.Arg (WrappedADT t x) x Int Bool

-- | The type of an unwrapped ADT arg
type UnwrappedArg t x = S.Arg (t x) x Int Bool

-- | The type of an extracted ADT
--
-- in future the forall will have to be dropped, as right now
-- we coerce `T a ~ T Int`
type Extracted t = forall x. WrappedADT t x

type ExtractedDUG = D.DUG
data WrappedADT t x =
  WrappedADT
      { nodeId :: Int
      , nodeArgs :: [ExtractArg t x]
      , nodeOp :: String
      , value :: UnwrappedArg t x
      }

instance Show (WrappedADT t x) where
  show (wadt) = intercalate " " ["WrappedADT", id, op, args]
   where id = show $ nodeId wadt
         args = show $ nodeArgs wadt
         op = nodeOp wadt

-- | During extraction of a DUG the program must keep track of some state
-- This state simply tracks the DUG as a list-of-births
data ExtractorState t x =
   ExtractorState
      { births :: [WrappedADT t x]
      , currentIndex :: Int
      }
   deriving (Show)

emptyExtractorState :: ExtractorState t x
emptyExtractorState = ExtractorState [] 0

state :: MVar (ExtractorState t x)
state = unsafePerformIO $ newEmptyMVar
{-# NOINLINE state #-}

unpickedArg :: ExtractArg t x -> D.DUGArg
unpickedArg (S.Version wadt) = S.Version (nodeId wadt)
unpickedArg (S.NonVersion (S.VersionParam i)) = S.NonVersion (S.VersionParam (unsafeCoerce i))
unpickedArg (S.NonVersion (S.IntArg i)) = S.NonVersion (S.IntArg i)
unpickedArg (S.NonVersion (S.BoolArg b)) = S.NonVersion (S.BoolArg b)

unpickedWrapped :: S.Signature -> WrappedADT t x -> (S.Operation, [D.DUGArg], Dynamic)
unpickedWrapped s wadt = (op, dargs, dyn)
   where op = (s^.S.operations) M.! (nodeOp wadt)
         dargs = map unpickedArg (nodeArgs wadt)
         dyn = undefined -- for extracted DUGs it is undefined to try evaluate them.

createDUGfromBirths :: S.Signature -> [WrappedADT t x] -> ExtractedDUG
createDUGfromBirths s births' = go (D.emptyDUG "extracted") births'
   where
      go d [] = d
      go d (b:bs) =
         let (sop, dargs, dyn) = unpickedWrapped s b in
         let d' = D.pushNew sop dargs dyn d in 
         go d' bs

extract :: S.Signature -> IO a -> IO (a, ExtractedDUG)
extract s a = do
   let st = emptyExtractorState
   print $ "putting..."
   putMVar state st
   print $ "put"
   v <- a
   print $ "done, taking..."
   st' <- takeMVar state
   print $ "took"
   let dug = createDUGfromBirths s (births st')
   return (v, dug)

_log_operation :: String -> [ExtractArg t x] -> t x -> WrappedADT t x
_log_operation opName args x = unsafePerformIO $ updateWrapper opName args (S.Version x)

_log_observer :: String -> [ExtractArg t x] -> x -> x
_log_observer opName args x = unsafePerformIO $ do
   _ <- updateWrapper opName args (S.NonVersion (S.VersionParam x)) -- TODO: this won't always be a version param
   return x

updateWrapper :: String -> [ExtractArg t x] -> UnwrappedArg t x -> IO (WrappedADT t x)
updateWrapper opName args v = do
   print $ ("updateWrapper", opName)
   (ExtractorState bs i) <- takeMVar state
   let w = WrappedADT i args opName v
   let st' = ExtractorState (w:bs) (i+1)
   putMVar state st'
   return w

getVersion :: WrappedADT t x -> t x
getVersion w = case value w of
   S.Version x -> x
   _ -> error "getVersion :: expected version arg"
