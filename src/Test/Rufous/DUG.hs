module Test.Rufous.DUG where

import qualified Data.Map as M

type Version = Int

data Arg =
     VersionNode Version
   | NonVersion  Int
   deriving (Show)

data DUG =
   DUG
      { versions :: [Version]
      , operations :: M.Map Version [Arg]
      }
   deriving (Show)