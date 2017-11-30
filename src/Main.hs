module Main where

import qualified Test.Rufous.DUG as D
import qualified Test.Rufous.Generate as G
import qualified Test.Rufous.Profile as P
import qualified Test.Rufous.Signature as S

s :: S.Signature [Int]
s =
    S.signature {
          S.tyName="T"
        , S.operations = [
              (S.operation   "enqueue"   "T a -> a -> T a")     { S.post=(\s [_, (S.Arg (Just a))] _ -> s ++ [a]) }
            , (S.operation   "dequeue"   "T a -> a")            { S.pre=(not . null), S.post=(\s _ _ -> tail s) }
            , (S.operation   "empty"     "T a")
        ]
        , S.initialState = []
    }

p :: P.Profile
p = P.Profile [("empty", 1), ("enqueue", 1), ("dequeue", 1)] 1 1

gen :: IO D.DUG1
gen = G.generateDUG s p

example_dug :: D.DUG1
example_dug =
    D.DUG1 {
        D.versions = [1, 2]
        , D.args = \i ->
            case i of
                1 -> [D.VersionNode 2]
                2 -> []
    }

main :: IO ()
main = putStrLn "Hello, Haskell!"
