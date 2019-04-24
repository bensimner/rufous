{-# LANGUAGE Rank2Types, BangPatterns #-}
module Test.Rufous.Extract where

import Debug.Trace

import Control.Lens

import Unsafe.Coerce
import System.IO.Unsafe
import Control.Concurrent.MVar

import Data.Dynamic (Dynamic)

import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import qualified Test.Rufous.DUG as D
import qualified Test.Rufous.Signature as S

import Data.List (intercalate)

-- | The type of an extracted ADT arg
type ExtractedArg t x = S.Arg (WrappedADT t x) x Int Bool

-- | The type of an unwrapped ADT arg
type UnwrappedArg t x = S.Arg (t x) x Int Bool

-- | The type of an extracted ADT
--
-- in future the forall will have to be dropped, as right now
-- we coerce `T a ~ T Int`
type Extracted t = forall x. WrappedADT t x

-- | Each Node has an ID
type Id = Int

type ExtractedDUG = D.DUG
data WrappedADT t x =
  WrappedADT
      { nodeId :: Id
      , nodeArg :: [ExtractedArg t x]
      , nodeOp :: String
      , value :: UnwrappedArg t x
      }
   deriving (Show)

data PartialDUG =
   Partial
      { partialNodes :: M.Map Id String            -- which nodes are which operations
      , partialArgs :: M.Map (Id, Int) D.DUGArg    -- (caller nodeId, argN) -> arg
      }
   deriving (Show)

-- | During extraction of a DUG the program must keep track of some state
-- This state simply tracks the DUG as a list-of-births
data ExtractorState t x =
   ExtractorState
      { partial :: PartialDUG
      , currentIndex :: Id
      }
   deriving (Show)

emptyPartialDUG :: PartialDUG
emptyPartialDUG = Partial M.empty M.empty

emptyExtractorState :: ExtractorState t x
emptyExtractorState = ExtractorState emptyPartialDUG 0

state :: MVar (ExtractorState t x)
state = unsafePerformIO $ newEmptyMVar
{-# NOINLINE state #-}

unwrapPartial :: S.Signature -> PartialDUG -> Id -> (S.Operation, [D.DUGArg], Dynamic)   
unwrapPartial s p i = (sop, dargs, dyn)
   where opName = (partialNodes p) M.! i
         sop = (s^.S.operations) M.! opName
         dargs = [fromMaybe (S.Version (-1)) (M.lookup (i,j) (partialArgs p)) | j <- [0 .. length (sop^.S.opArgTypes) - 1]]
         dyn = error "evaluating an extracted DUG directly is undefined."

dugFromPartial :: S.Signature -> PartialDUG -> D.DUG
dugFromPartial s p = go (D.emptyDUG "extracted") (M.keys (partialNodes p))
   where
      go d [] = d
      go d (i:is) =
         let (sop, dargs, dyn) = unwrapPartial s p i in
         let d' = D.pushNew sop dargs dyn d in
         go d' is

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
   print $ "partial=" ++ show (partial st')
   let dug = dugFromPartial s (partial st')
   return (v, dug)

{-# NOINLINE _const #-}
-- | _const is used to defeat GHC's heuristics trying to pull the unsafe block into a CAF.
_const :: Id -> a -> Id
_const curId _ = curId

{-# NOINLINE _get_id #-}
_get_id :: a -> Id
_get_id x = unsafePerformIO $ do
   (ExtractorState p curId) <- takeMVar state
   print ("_get_id", curId)
   putMVar state (ExtractorState p (curId+1))
   return $ curId

_log_operation :: Id -> String -> t x -> WrappedADT t x
_log_operation curId opName x = unsafePerformIO $ do
   print ("_log_operation", curId, opName)
   updateWrapper curId opName (S.Version x)

_log_observer :: Id -> String -> x -> x
_log_observer curId opName x = unsafePerformIO $ do
   print ("_log_observer", curId, opName)
   _ <- updateWrapper curId opName (S.NonVersion (S.VersionParam (unsafeCoerce x))) -- TODO: this won't always be a version param
   return x

updateWrapper :: Id -> String -> UnwrappedArg t x -> IO (WrappedADT t x)
updateWrapper curId opName v = do
   print $ ("updateWrapper", curId, opName)
   (ExtractorState (Partial pnodes pargs) i) <- takeMVar state
   let w = WrappedADT curId [] opName v
   let p' = Partial (M.insert curId opName pnodes) pargs
   let st' = ExtractorState p' i
   putMVar state st'
   return $ w

updateArg :: Id -> Int -> D.DUGArg -> IO ()
updateArg parentId argId darg = do
   print ("unwrap", parentId, argId)
   (ExtractorState (Partial pnodes pargs)  curId) <- takeMVar state
   let p' = Partial pnodes (M.insert (parentId, argId) darg pargs)
   let st' = ExtractorState p' curId
   putMVar state st'

unwrap :: Id -> Int -> WrappedADT t x -> t x
unwrap parentId argId w = case value w of
   S.Version x -> seq update x
   _ -> error "unwrap :: expected version arg"
   where update = unsafePerformIO $ updateArg parentId argId (S.Version (nodeId w))

nonversion :: Id -> Int -> ExtractedArg t x -> a -> a
nonversion parentId argId (S.NonVersion nva) x = seq update x
   where update = unsafePerformIO $ updateArg parentId argId (S.NonVersion nva')
         nva' = case nva of
            S.VersionParam i -> S.VersionParam (unsafeCoerce i)
            S.IntArg i -> S.IntArg i
            S.BoolArg b -> S.BoolArg b
{-# NOINLINE nonversion #-}

{-

class Listy t where
   listcons :: a -> t a -> t a
   listempty :: t a
   listhead :: t a -> a

instance Listy t => Listy (WrappedADT t) where
   listcons x xs =
      let curId = _get_id() in
      _log_operation curId "listcons" (listcons (nonversion curId 0 (NonVersion (VersionArg x)) x) (unwrap curId 1 xs))
-}
