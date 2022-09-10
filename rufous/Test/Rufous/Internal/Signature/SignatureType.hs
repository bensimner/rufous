{-# LANGUAGE StandaloneDeriving, TemplateHaskell, ExistentialQuantification #-}
module Test.Rufous.Internal.Signature.SignatureType where

import Control.Lens
import Data.Typeable (Typeable)

import qualified Data.Map as M
import Data.Dynamic (Dynamic)

import Test.Rufous.Internal.Signature.OperationType
import Test.Rufous.Internal.Signature.ImplementationType

data Null x = NullImpl
  deriving (Show, Eq, Typeable)

-- | Finally, the Signature of the entire datatype is a simply a set of operations, a set of
-- implementations (including the named shadow and null implementations).
--
-- Note that the null implementation is a Maybe, but /must/ be filled in to be able to do evaluation
data Signature =
  Signature
      { _signatureADTName :: String
      , _operations :: M.Map String Operation

      -- | these functions get used in forcing the evaluation of the result of an observer
      -- (if it exists)
      , _opObsForcers :: M.Map String Dynamic
      , _implementations :: [Implementation]
      , _nullImpl :: Maybe Implementation
      , _shadowImpl :: Maybe ShadowImplementation
      }
  deriving (Show)
makeLenses ''Signature

-- Below are functions that can be chained together to build up a signature from an empty one

-- | Updates a Signature with additional implementations
addImpl :: Implementation -> Signature -> Signature
addImpl impl s = s & implementations %~ (impl:)


-- | Sets the null implementation
setNull :: Implementation -> Signature -> Signature
setNull impl s = s & nullImpl .~ Just impl


-- | Sets the shadow implementation
setShadow :: ShadowImplementation -> Signature -> Signature
setShadow impl s = s & shadowImpl .~ Just impl