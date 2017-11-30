module Test.Rufous.DUG where

import qualified Test.Rufous.Signature as Sig

data DUGArg =
      VersionNode Int
    | NonVersionArg Int

    deriving (Show)

data DUG1 =
    DUG1 {
        versions :: [Int]
        , args :: Int -> [DUGArg]
    }

instance Show DUG1 where
    -- naive pretty print DUG
    show (DUG1 v args) = unlines [show a ++ " -> " ++ (show $ args a) | a <- v]