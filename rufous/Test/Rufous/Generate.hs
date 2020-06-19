{-# LANGUAGE TemplateHaskell #-}
module Test.Rufous.Generate where

import Control.Lens

import System.Random
import Control.Monad.State

import qualified Test.Rufous.Options as Opt
import qualified Test.Rufous.Signature as S
import qualified Test.Rufous.Profile as P
import qualified Test.Rufous.DUG as D

import Test.Rufous.Internal.Generate.Types
import Test.Rufous.Internal.Generate.Core

import Test.Rufous.Internal.Logger as Log

precheck :: Opt.RufousOptions -> S.Signature -> P.Profile -> IO ()
precheck _ s _ = do
   case s^.S.shadowImpl of
      Just _  -> return ()
      Nothing -> fail $
       "Rufous: could not find Shadow implementation for " ++ s^.S.signatureADTName
       ++ ". A Shadow is required for DUG generation."

-- | Generates a 'DUG' which conforms to a given Profile.
generateDUG :: Opt.RufousOptions -> S.Signature -> P.Profile -> IO D.DUG
generateDUG o s p = do
      Log.debug $ "generating of size " ++ show (p^.P.size)
      precheck o s p

      stdgen <- getStdGen
      let (nameUUID, gen') = randomR (999 :: Int, 1000000) stdgen
      let freshName = "dug" ++ (show nameUUID)
      let size = p^.P.size
      let (a, st') = runState (build size) (emptyGenSt o s p gen' freshName)
      setStdGen (st'^.gen)
      return a