{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Map as M

import Control.Lens

import Test.Rufous
import Test.Rufous.Generate
import Test.Rufous.Signature
import Test.Rufous.SigConstructor
import Test.Rufous.Profile

p = 
   Profile 
      { _operationWeights=M.fromList [("snoc", 1/2), ("empty", 1/2)]
      , _persistentApplicationWeights=M.fromList [("snoc", 1/2), ("empty", 1/1)]
      , _mortality=1/2
      }
{-
s =
   signature 
      [ operation "snoc :: a -> T a -> T a" 
      , operation "empty :: T a"
      ]
   & implementations .~ []

main2 :: IO ()
main2 = do
   let genState = emptyState s p 
   putStrLn $ pprintGenState genState
   inflated <- inflate genState
   putStrLn $ pprintGenState inflated
   deflated <- deflate inflated
   putStrLn $ pprintGenState deflated
   print $ deflated
-}

main :: IO ()
main = do
   let path = "./examples/Queue.hs"
   r <- readModuleFile path
   case r of
      Right sig -> do
         print sig
         let genState = emptyState sig p
         putStrLn $ pprintGenState genState
         inflated <- inflate genState
         putStrLn $ pprintGenState inflated
         deflated <- deflate inflated
         putStrLn $ pprintGenState deflated
         print $ deflated
      Left  (Failure src msg) -> do
         print msg
         print src
