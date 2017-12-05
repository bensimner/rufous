module Test.Rufous.Generate where

import Control.Monad
import System.Random

import qualified Data.Map as M

import Test.Rufous.RndUtil

import qualified Test.Rufous.Signature as S
import qualified Test.Rufous.Profile as P

data DUGArg = Version Int | NonVersion Int
   deriving (Eq, Show)

type VersionNode = [DUGArg]

data BufferedOperation =
   BufferedOperation
      { bufOpName :: String
      , bufArgs   :: [DUGArg]
      , remaining :: [S.Arg]
      }
      deriving (Eq, Show)

data GenDug =
   GenDug
      { versions :: [VersionNode]         -- the DUG built so far
      , operations :: [BufferedOperation] -- operations yet to add
      }
      deriving (Eq, Show)

emptyDug :: GenDug
emptyDug = GenDug [] []

-- add a thing to the DUG
inflateDug :: S.Signature -> P.Profile -> GenDug -> IO GenDug
inflateDug s p d = do
   let typeWeights = [ (inflateDug_Operation s p (P.normaliseWeights $ P.mutatorWeights p) d, P.pMutator p)
                     , (inflateDug_Operation s p (P.normaliseWeights $ P.observerWeights p) d, P.pObserver p)
                     , (inflateDug_Operation s p (P.normaliseWeights $ P.generatorWeights p) d, P.pGenerator p) ]
   join $ chooseRandom typeWeights

inflateDug_Operation :: S.Signature -> P.Profile -> P.ProfileEntry -> GenDug -> IO GenDug
inflateDug_Operation s p m d = do
   opName <- chooseOperation m
   let o = BufferedOperation opName [] (S.opArgs $ S.sig $ S.operations s M.! opName)
   return $ d { operations=o : operations d }

tryCreateFromBuffered :: GenDug -> IO GenDug
tryCreateFromBuffered d = do
   (ops, vs) <- new (operations d) (versions d)
   return $ GenDug vs ops
   where
      new :: [BufferedOperation] -> [VersionNode] -> IO ([BufferedOperation], [VersionNode])
      new (op : ops) vs = do
         (op', vs') <- tryFillOp op vs
         (ops', vs'') <- new ops vs'
         case op' of
            Just op'' -> return (op'' : ops', vs'')
            Nothing   -> return (       ops', vs'')
      new [] vs = return ([], vs)

tryFillOp :: BufferedOperation -> [VersionNode] -> IO (Maybe BufferedOperation, [VersionNode])
tryFillOp bOp vs = do
   (args, rem) <- go (remaining bOp)
   let newArgs = bufArgs bOp ++ args
   if null rem then
      return $  (Nothing, newArgs : vs)
   else do
      let bufOp' = BufferedOperation (bufOpName bOp) (bufArgs bOp ++ args) (rem)
      return (Just bufOp', vs)
   where
      go :: [S.Arg] -> IO ([DUGArg], [S.Arg])
      go (a : as) =
         case a of
            S.NonVersion -> do
               r <- chooseNonVersion
               (args, rem) <- go as
               return (NonVersion r : args, rem)
            S.Version ->
               if not $ null vs then do
                  v <- chooseUniform $ [0 .. (length vs) - 1]
                  (args, rem) <- go as
                  return (Version v : args, rem)
               else
                  return ([], [])

      go [] = return ([], [])

generate :: S.Signature -> P.Profile -> IO GenDug
generate s p = do
   k <- randomRIO (5, 25)
   d' <- build emptyDug k
   flatten d'
   where
      build :: GenDug -> Int -> IO GenDug  -- I do not know why I need this?
      build d 0 = return d
      build d k = do
         d' <- inflateDug s p d
         build d' (k - 1)

      -- now run tryCreateFromBuffered until fixed point is hit
      flatten :: GenDug -> IO GenDug
      flatten d = do
         d' <- tryCreateFromBuffered d
         if d == d' then
            return d'
         else
            flatten d'