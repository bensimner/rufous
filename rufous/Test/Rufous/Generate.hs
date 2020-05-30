{-# LANGUAGE TemplateHaskell #-}
module Test.Rufous.Generate where

import Control.Lens

import System.Random
import Control.Monad.State

import Data.Foldable (toList)

import qualified Test.Rufous.Options as Opt
import qualified Test.Rufous.Signature as S
import qualified Test.Rufous.Profile as P
import qualified Test.Rufous.DUG as D

import Test.Rufous.Internal.Generate.Types
import Test.Rufous.Internal.Generate.Core

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
      precheck o s p
      name <- freshName
      let size = p^.P.size
      let (a, s') = runState (build size) (emptyGenSt o s p name)
      if Opt.debug o then
         mapM_ putStrLn (toList (s'^.dbg^.dbgTrace))
      else return ()
      return a
   where
      freshName = do
         n <- randomRIO (999 :: Int, 10000000)
         return $ "dug" ++ (show n)


