{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Map

import System.IO.Unsafe

import qualified Test.Rufous.DUG as D
import qualified Test.Rufous.Generate as G
import qualified Test.Rufous.Profile as P
import qualified Test.Rufous.Signature as S
import qualified Test.Rufous.Eval as E

impl1 = 
   S.implementation "Data.Set.Set"
      [ ("empty", "Data.Set.empty")
      , ("enqueue", "\\t a -> Data.Set.insert a t")
      , ("dequeue", "\\t   -> Data.Set.elemAt 0 t")
      ]
s =
    S.signature "T"
        [ S.operation "enqueue" "T a -> a -> T a"
        , S.operation "dequeue" "T a -> a"
        , S.operation "empty" "T a"]
        [ impl1 ]

p =
    P.Profile
        { P.mutatorWeights = fromList [("enqueue", 3/6), ("dequeue", 2/6)]
        , P.generatorWeights = fromList [("empty", 1/6)]
        , P.observerWeights = fromList []
        , P.persistentMutationWeight = 0.5
        , P.persistentObservationWeight = 0.5
        , P.mortality = 0.3
        }

gend = unsafePerformIO $ G.generate s p (5000, 10000)
d = G.genDug2DUG gend
showd = D.dug2dot d

main :: IO ()
main = do
   gend <- G.generate s p (5, 10)
   putStrLn "generated DUG"
   let d = G.genDug2DUG gend
   putStrLn " DUG converted"
   D.dug2dot d
   putStrLn "written dot, drawing graphviz in background..."
   putStrLn "running DUG on impl1"
   r <- E.runDUG s d impl1 
   case r of 
      Left s -> putStrLn "** ERROR ** " >> putStrLn s
      Right tr -> print tr
