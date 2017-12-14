{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Data.Map as M

import System.IO.Unsafe

import qualified Test.Rufous.DUG as D
import qualified Test.Rufous.Generate as G
import qualified Test.Rufous.Profile as P
import qualified Test.Rufous.Signature as S
import qualified Test.Rufous.Eval as E

-- Eventually I'll convert the Datatypes to use Typeable
-- and then can pass the Haskell expressions in rather than parsing strings
-- and generating extra code afterwards
--
-- Sets are a good example because operations have pre-conditions on both version and non-version arguments
-- as well as having a good mix of simple observers/generators/mutators
-- and have very different performance depending on profile
--    (insert is cheap, remove/lookup is expensive)
--

impl1 = 
   S.implementation "Data.Set.Set"
      [ ("empty", "Data.Set.empty")
      , ("add", "\\t a -> Data.Set.insert a t")
      , ("elemAt", "\\t x -> Data.Set.elemAt x t")
      , ("deleteAt", "\\t x -> Data.Set.deleteAt x t")
      ]

impl2 = 
   S.implementation "Data.List.List"
      [ ("empty", "[]")
      , ("add", "\\t a -> a : t")
      , ("elemAt", "\\t i -> t !! i")
      , ("deleteAt", "\\(x:xs) -> xs")
      ]

-- The pre-conditions here are overly resctive to eliminate the problem of non-determinism 
-- interacting with uniqueness
-- i.e. (add 1 (removeAt 0 (fromList [1, 2])))  may or may not be valid at runtime
s =
    (S.signature "T"
        [ (S.operation "add" "T a -> a -> T a") 
            { S.pre        = \[S.VersionArg xs, S.IntArg k] -> not (k `elem` xs)  -- uniqueness
            , S.transition = \[S.VersionArg xs, S.IntArg k] -> (k:xs)
            }
        , (S.operation "deleteAt" "T a -> a -> T a") 
            { S.pre        = \[S.VersionArg xs, S.IntArg k] -> k >= 0 && k < length xs
            , S.transition = \[S.VersionArg xs, S.IntArg k] -> xs  -- we do not know which was removed, 
                                                                   -- need non-deterministic postconditions
            }
        , (S.operation "elemAt" "T a -> a -> a") 
            { S.pre        = \[S.VersionArg xs, S.IntArg k] -> k >= 0 && k < length xs
            }
        , S.operation "empty" "T a"]
        [ impl1 ])
        { S.initialState=[] }

p =
    P.Profile
        { P.mutatorWeights = M.fromList [("add", 2/6), ("deleteAt", 1/6)]
        , P.generatorWeights = M.fromList [("empty", 1/6)]
        , P.observerWeights = M.fromList [("elemAt", 2/6)]
        , P.persistentMutationWeight = 0.5
        , P.persistentObservationWeight = 0.5
        , P.mortality = 0.3
        }

main :: IO ()
main = do
   gend <- G.generate s p (5, 10)
   putStrLn "generated DUG"
   print gend
   let d = G.genDug2DUG gend
   putStrLn " DUG converted"
   print d
   D.dug2dot d
   putStrLn "written dot, drawing graphviz in background..."
   putStrLn "running DUG on impl1"
   r <- E.tryEvaluateDUG s d impl1 
   case r of 
      Left s -> putStrLn "** ERROR ** " >> putStrLn s
      Right tr -> print tr
