{-# LANGUAGE GADTs, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}

module Main where

import qualified Data.Map as M
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL


import Control.Lens hiding (snoc)

import Test.Rufous
import qualified Test.Rufous.DUG as D
import Test.Rufous.Signature
import Test.Rufous.Generate
import Test.Rufous.TH
import Test.Rufous.Profile
import Test.Rufous.Run
import Test.Rufous.Extract
import Test.Rufous.Stats
import Control.Exception
import Test.Rufous.Exceptions

import Language.Haskell.TH

import Data.Maybe
import Data.Dynamic
import Debug.Trace

p = 
   Profile 
      { _operationWeights=M.fromList [("snoc", 3/8), ("empty", 1/8), ("head'", 1/4), ("tail'", 1/4)]
      , _persistentApplicationWeights=M.fromList [("snoc", 1/2), ("empty", 1/1), ("head'", 1/1), ("tail'", 1/1)]
      , _mortality=20/100
      }

class QueueADT q where
   snoc :: a -> q a -> q a
   empty :: q a
   head' :: q a -> a
   tail' :: q a -> q a

instance QueueADT [] where
   snoc x xs = xs ++ [x]
   empty = []
   head' = head
   tail' = tail

-- A Shadow is generally a valid implementation tagged with some 
-- additional information
data Shadow x = Shadow Int
   
instance Show (Shadow x) where
   show (Shadow i) = show i

instance QueueADT Shadow where
   snoc x (Shadow q) = Shadow (q + 1)
   empty = Shadow 0

   tail' (Shadow 0) = throw GuardFailed
   tail' (Shadow q) = Shadow (q - 1)

   head' (Shadow 0) = throw GuardFailed
   head' (Shadow q) = throw NotImplemented

makeRufousSpec ''QueueADT

example_program :: IO ()
example_program = do
   let q0 = (empty :: WrappedADT [] Int)
   let q1 = snoc 1 q0
   print $ head' q1
   let q2 = snoc 2 q1
   let q3 = snoc 3 q1
   print $ head' (tail' q2)
   print $ head' (tail' q3)

main_generate :: IO ()
main_generate = do
   dug <- makeDUG _QueueADT p 5000
   --gendug2dot _QueueADT dug False "tmp"
   --gendug2dot _QueueADT dug True "tmp2"
   tr <- runDUG ((_QueueADT ^. implementations) !! 0) dug
   nimp <- runDUG (_QueueADT ^. nullImpl) dug
   putStrLn $ "Time " ++ show (sum tr - sum nimp)


main_experiment :: IO ()
main_experiment = do
--   experiment1 _QueueADT
--   experiment2 _QueueADT
   experiment3 _QueueADT


main_extract :: IO ()
main_extract = do
   init_state
   example_program
   dug <- read_state _QueueADT
   D.dug2dot (dug) ("tmp")
   print $ D.extractProfile _QueueADT dug

main = main_experiment
