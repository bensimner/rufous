{-# LANGUAGE StandaloneDeriving, TemplateHaskell, ExistentialQuantification #-}
module Test.Rufous.Internal.Signature.Types where

import Data.Dynamic (Dynamic, Typeable)
import Control.Lens

import qualified Data.Map as M

data Null x = NullImpl
  deriving (Show, Eq, Typeable)


-- | Each argument in a DUG is either a version argument, pointing to another node in the DUG,
-- or a NonVersion argument.  Each NonVersion argument can be one of a fixed set of
-- monomorphic types or the polymorphic argument to the datatype (the so-called Version
-- Parameter)

data Arg v n i b = Version v | NonVersion (NVA n i b)
  deriving (Eq, Show)
data NVA n i b = VersionParam n | IntArg i | BoolArg b
  deriving (Eq, Show)

-- TODO: it would be good to replace the IntArg/BoolArg constructors with a simple
type ArgType = Arg () () () ()

-- | Each operation is categorized into one of a Mutator, Observer or Generator.
-- Generators are operations that produce versions from arguments:
--     empty :: List a
--     singleton :: a -> List a
--     new :: Int -> Queue a
-- 
-- Mutators are operations that take a version and modify it, returning the new one:
--     cons :: a -> List a -> List a
--     snoc :: Queue a -> a -> Queue a
--     diff :: Set a -> Set a -> Set a
-- 
-- Observers are those that take version arguments and force a value out of them:
--     head :: List a -> a
--     size :: Queue a -> Int
--     null :: List a -> Bool
data OperationCategory = Mutator | Observer | Generator
   deriving (Eq, Show, Ord)

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


-- | Finally, the Signature of the entire datatype is a simply a set of operations, a set of
-- implementations (including the named shadow and null implementations).
data Signature =
  Signature
      { _operations :: M.Map String Operation
      , _implementations :: [Implementation]
      , _nullImpl :: Implementation
      , _nullExtractorImpl :: Implementation
      , _shadowImpl :: Maybe ShadowImplementation
      }
  deriving (Show)

-- | Each 'Operation' is the function name and its signature.
data Operation =
  Operation
      { _opName :: String
      , _opRetType :: ArgType
      , _opArgTypes :: [ArgType]
      , _opCategory :: OperationCategory
      }
  deriving (Show)
makeLenses ''Operation
makeLenses ''Signature
