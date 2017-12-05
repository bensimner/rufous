module Test.Rufous.SigParser
   ( OperationType(..)
   , Arg(..)
   , OperationSig(..)
   , parseSig
   ) where

import Text.Parsec

data OperationType = Mutator | Observer | Generator
   deriving (Show)

data Arg = Version | NonVersion
   deriving (Eq, Show)

data OperationSig =
   OperationSig
      { opArgs :: [Arg]
      , opType :: OperationType
      }
   deriving (Show)

-- this is slightly hardcoded for now.
parseSig :: String -> OperationSig
parseSig s =
   case s of
      "T a"             -> OperationSig [] Generator
      "T a -> a"        -> OperationSig [Version] Observer
      "T a -> a -> T a" -> OperationSig [Version, NonVersion] Mutator
