module Test.Rufous.Signature
   ( Operation(..)
   , Signature(..)
   , signature
   , operation
   , Arg(..)
   , OperationType(..)
   , OperationSig(..)
   ) where

import qualified Data.Map as M
import Test.Rufous.SigParser

data Operation =
   Simple
      { opName :: String
      , sig    :: OperationSig
      }
   deriving (Show)

data Signature =
   Signature
      { operations :: M.Map String Operation
      }
   deriving (Show)

signature :: String -> [Operation] -> Signature
signature name ops = Signature (M.fromList [(opName o, o) | o <- ops])

operation :: String -> String -> Operation
operation name typeSig = Simple name (parseSig typeSig)