{-# LANGUAGE StandaloneDeriving, TemplateHaskell, ExistentialQuantification #-}
module Test.Rufous.Internal.Signature.OperationType where

import Control.Lens

import Test.Rufous.Internal.Signature.SimpleGrammar

-- | Each 'Operation' is the function name and its signature.
-- its signature comes from the 'Simple' grammar defined above
data Operation =
  Operation
      { _opName :: String
      , _opRetType :: ArgType
      , _opArgTypes :: [ArgType]
      , _opCategory :: OperationCategory
      }
  deriving (Show)
makeLenses ''Operation
