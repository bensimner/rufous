{-# LANGUAGE TemplateHaskell #-}
module Test.Rufous.Generate where

import System.Random
import Control.Monad.State

import qualified Data.Map as M
import qualified Data.Set as St

import qualified Test.Rufous.Signature as S
import qualified Test.Rufous.Profile as P
import qualified Test.Rufous.DUG as D

import Test.Rufous.Internal.Generate.Types
import Test.Rufous.Internal.Generate.Core

-- | Generates a 'DUG' which conforms to a given Profile.
generateDUG :: S.Signature -> P.Profile -> Int -> IO D.DUG
generateDUG s p size = do
      name <- freshName
      return $ evalState (build size) (emptyGenSt s p name)
   where
      freshName = do
         n <- randomRIO (999 :: Int, 10000000)
         return $ "dug" ++ (show n)


