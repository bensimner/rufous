{-# LANGUAGE ExistentialQuantification #-}
module Test.Rufous.Internal.Evaluation.Types where

import Control.Exception
import Data.Typeable

import qualified Test.Rufous.Signature as S

-- | Control flow between implementations and rufous is controlled
-- via exceptions. 
-- a 'GuardFailed' exception indicates that the precondition on some
-- fully-satisifed version in the DUG failed.
-- NotImplemneted means there is no valid implementation for this
-- operation (such as for observers in shadows).
data RufousException =
      GuardFailed | NotImplemented
   deriving (Show, Typeable)
instance Exception RufousException

-- | When Running a node in a DUG there are multiple outcomes
data RunResult =
   forall a. Typeable a =>
      RunSuccess !a  | RunTypeMismatch | RunExcept RufousException
