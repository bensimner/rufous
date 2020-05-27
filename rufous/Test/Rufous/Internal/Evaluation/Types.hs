{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
module Test.Rufous.Internal.Evaluation.Types where

import Control.Lens

import Data.Time.Clock
import Control.Exception
import Data.Typeable

import qualified Data.Map as M

import qualified Test.Rufous.Signature as S
import qualified Test.Rufous.DUG as D
import qualified Test.Rufous.Profile as P

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

-- | 'TimingInfo' contains the time for a DUG, for each implementation
data TimingInfo =
   TInfo
      { _nullTime :: NominalDiffTime
      , _times :: M.Map S.Implementation NominalDiffTime
      }
   deriving (Show)
makeLenses ''TimingInfo

-- |  A 'Result' is the information from a single DUG run.
data Result =
   Result
      { _resultDUG :: D.DUG
      , _resultProfile :: P.Profile
      , _resultOpCounts :: M.Map String Int
      , _resultTimes :: TimingInfo
      }
   deriving (Show)
makeLenses ''Result
