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
        { P.mutatorWeights = fromList [("enqueue", 1/3), ("dequeue", 1/3)]
        , P.generatorWeights = fromList [("empty", 1/3)]
        , P.observerWeights = fromList []
        , P.persistentMutationWeight = 0.5
        , P.persistentObservationWeight = 0.5
        }

gen = G.generate s p

{-
example_dug :: D.DUG1
example_dug =
    D.DUG1 {
        D.versions = [1, 2]
        , D.args = \i ->
            case i of
                1 -> [D.VersionNode 2]
                2 -> []
    }
-}

main :: IO ()
main = putStrLn "Hello, Haskell!"
