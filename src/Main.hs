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

impl1 = 
   S.implementation "[]"
      [ ("empty", "[]")
      , ("snoc", "\\t a -> t ++ [a]")
      , ("tail'", "\\(t:ts) -> ts")
      , ("head'", "\\(t:ts) -> t")
      ]

queue =
    (S.signature "Q"  -- the type variable to use
        [ (S.operation "snoc" "T a -> a -> T a") 
            { S.pre        = const True
            , S.transition = \[S.VersionArg xs, S.IntArg k] -> xs ++ [k]
            }
        , (S.operation "tail'" "T a -> T a") 
            { S.pre        = \[S.VersionArg xs] -> length xs > 0
            , S.transition = \[S.VersionArg (x:xs)] -> xs
            }
        , (S.operation "head'" "T a -> a") 
            { S.pre        = \[S.VersionArg xs] -> length xs > 0
            , S.transition = \[S.VersionArg xs] -> xs
            }
        , S.operation "empty" "T a"]
        [ impl1 ])
        { S.initialState=[] }

p =
    P.Profile
        { P.mutatorWeights = M.fromList [("snoc", 2/6), ("tail'", 1/6)]
        , P.generatorWeights = M.fromList [("empty", 1/6)]
        , P.observerWeights = M.fromList [("head'", 2/6)]
        , P.persistentMutationWeight = 0.5
        , P.persistentObservationWeight = 0.5
        , P.mortality = 0.5
        }

main :: IO ()
main = do
   gend <- G.generate queue p (50, 100)
   putStrLn "generated DUG"
   print gend
   let d = G.genDug2DUG gend
   putStrLn " DUG converted"
   print d
   D.dug2dot d
   putStrLn "written dot, drawing graphviz in background..."
   putStrLn "running DUG on impl1"
   r <- E.tryEvaluateDUG queue d impl1 
   case r of 
      Left s -> putStrLn "** ERROR ** " >> putStrLn s
      Right tr -> print tr
