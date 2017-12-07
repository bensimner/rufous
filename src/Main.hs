{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Map

import qualified Test.Rufous.DUG as D
import qualified Test.Rufous.Generate as G
import qualified Test.Rufous.Profile as P
import qualified Test.Rufous.Signature as S

s =
    S.signature "T"
        [ S.operation "enqueue" "T a -> a -> T a"
        , S.operation "dequeue" "T a -> a"
        , S.operation "empty" "T a"]

p =
    P.Profile
        { P.mutatorWeights = fromList [("enqueue", 3/6), ("dequeue", 2/6)]
        , P.generatorWeights = fromList [("empty", 1/6)]
        , P.observerWeights = fromList []
        , P.persistentMutationWeight = 0.5
        , P.persistentObservationWeight = 0.5
        }

gen = G.genDug2DUG <$> G.generate s p >>= D.dug2dot

main :: IO ()
main = putStrLn "Hello, Haskell!"
