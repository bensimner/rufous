{-# LANGUAGE StandaloneDeriving, TemplateHaskell, ExistentialQuantification #-}
module Test.Rufous.Internal.Signature.ImplementationType where

import Data.Dynamic (Dynamic, Typeable)
import Control.Lens

import qualified Data.Map as M

-- | An 'Implementation' is simply a set of operations with a reference to the actual
-- functions.
--
-- This implementation of 'Implementation's is type unsafe.
data ImplType = forall t. Typeable t => ImplType t
data Implementation =
  Implementation
      { _implName :: String
      , _implOperations :: M.Map String (Dynamic, ImplType)
      }
makeLenses ''Implementation

instance Show Implementation where
  show (Implementation impl _) = "<" ++ impl ++ ">"
instance Eq Implementation where
  i1 == i2 = i1^.implName == i2^.implName
instance Ord Implementation where
  i1 <= i2 = i1^.implName <= i2^.implName

-- | A Shadow implementation is merely another implementation
type ShadowImplementation = Implementation