{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
module Test.Rufous.Internal.Evaluation.Types where

import Control.Lens

import Data.Time.Clock
import Control.Exception
import Data.Typeable

import qualified Data.Map as M

import Data.List (intercalate)

import qualified Test.Rufous.Signature as S
import qualified Test.Rufous.Internal.DUG.Types as D
import qualified Test.Rufous.Profile as P

-- | Control flow between implementations and rufous is controlled
-- via exceptions.
-- a 'GuardFailed' exception indicates that the precondition on some
-- fully-satisifed version in the DUG failed.
-- NotImplemneted means there is no valid implementation for this
-- operation (such as for observers in shadows).
data RufousEvalException =
      GuardFailed | NotImplemented
   deriving (Show, Typeable)
instance Exception RufousEvalException

-- | When Running a node in a DUG there are multiple outcomes
data RunResult =
   forall a. Typeable a =>
      RunSuccess !a
   | RunShadowTypeMismatch
   | RunShadowFailure S.Implementation D.Node String
   | RunTypeMismatch TypeRep TypeRep
   | RunExcept RufousEvalException

instance Show RunResult where
   show (RunSuccess a) = "RunSuccess <" ++ show (typeOf a) ++ ">"
   show RunShadowTypeMismatch = "RunShadowTypeMismatch"
   show (RunShadowFailure a b c) = "RunShadowFailure " ++ intercalate " " [show a, show b, show c]
   show (RunTypeMismatch _ _) = "RunTypeMismatch"
   show (RunExcept e) = "RunExcept " ++ show e

-- | 'TimingInfo' contains the time for a DUG, for each implementation
data TimingInfo =
   TInfo
      { _nullTime :: NominalDiffTime
      , _times :: M.Map S.Implementation NominalDiffTime
      }
   deriving (Show)
makeLenses ''TimingInfo

-- | A Failed execution is tagged with a message
data ResultFailure =
      ResultFail String
   deriving (Show)

-- | Evaluation information from a single run of a particular DUG
data DUGTimeInfo =
     DUGEvalFail ResultFailure
   | DUGEvalTimes TimingInfo
   deriving (Show)
makeLenses ''DUGTimeInfo

-- | Information from many executions of the same DUG
data Result =
   Result
      { _resultProfile :: P.Profile
      , _resultOpCounts :: M.Map String Int
      , _resultAvgTimes :: DUGTimeInfo
      , _resultAllTimings :: [DUGTimeInfo]
      }
   deriving (Show)
makeLenses ''Result