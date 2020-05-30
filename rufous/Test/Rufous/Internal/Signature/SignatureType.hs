{-# LANGUAGE StandaloneDeriving, TemplateHaskell, ExistentialQuantification #-}
module Test.Rufous.Internal.Signature.SignatureType where

import Control.Lens
import Data.Typeable (Typeable)

import qualified Data.Map as M

import Test.Rufous.Internal.Signature.OperationType
import Test.Rufous.Internal.Signature.ImplementationType

data Null x = NullImpl
  deriving (Show, Eq, Typeable)

-- | Finally, the Signature of the entire datatype is a simply a set of operations, a set of
-- implementations (including the named shadow and null implementations).
data Signature =
  Signature
      { _signatureADTName :: String
      , _operations :: M.Map String Operation
      , _implementations :: [Implementation]
      , _nullImpl :: Implementation
      , _nullExtractorImpl :: Implementation
      , _shadowImpl :: Maybe ShadowImplementation
      }
  deriving (Show)
makeLenses ''Signature
