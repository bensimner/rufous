{-# LANGUAGE TemplateHaskell #-}
module Test.Rufous.Internal.Generate.Types where

import Control.Monad.State
import Control.Lens
import System.Random (StdGen, mkStdGen)

import qualified Data.Sequence as Sq
import qualified Data.Set as St

import qualified Test.Rufous.DUG as D
import qualified Test.Rufous.Profile as P
import qualified Test.Rufous.Signature as S

import qualified Test.Rufous.Internal.Generate.MSet as MSt

data PersistenceType = Persistent | Ephemeral
   deriving (Show)

-- | Each argument to a buffered operation is either an abstract one, that is yet
-- to be assigned.  Or a concrete one tagged with its value (or node from the DUG).
data BufferedArg =
     Abstract S.ArgType PersistenceType
   | Concrete D.DUGArg
   deriving (Show)

-- | An operation as yet uncommitted to the DUG is 'buffered'.
-- such buffered operations contains the underlying operation signature
-- as well as the current set of decided/undecided arguments.
data BufferedOperation =
   BufferedOperation
      { _bufOp :: S.Operation
      , _bufArgs :: [BufferedArg]
      , _life :: Int
      }
   deriving (Show)
makeLenses ''BufferedOperation

-- | When generating we often want to create arguments to 
data NodeBucket =
   NodeBucket
      { _infants :: MSt.MSet Int
      , _persistents :: MSt.MSet Int
      }
makeLenses ''NodeBucket

-- | During generation there is a lot of state that is kept:
--    - the current partially built DUG
--    - the buffer of uncommitted operations
--    - the set of not-dead nodes
-- Along with any options for the creation
data GenSt =
   GenSt
      { _sig :: S.Signature
      , _profile :: P.Profile
      , _buffer :: Sq.Seq BufferedOperation
      , _dug :: D.DUG
      , _living :: St.Set Int
      , _mutators :: NodeBucket
      , _observers :: NodeBucket
      , _gen :: StdGen
      }
makeLenses ''GenSt

-- | An empty node bucket has empty infant/persistent sets
emptyNodeBucket = NodeBucket MSt.empty MSt.empty

emptyGenSt :: S.Signature -> P.Profile -> String -> GenSt
emptyGenSt s p name = GenSt s p Sq.empty d St.empty emptyNodeBucket emptyNodeBucket (mkStdGen 0)  -- TODO: seed it better...
   where d = D.emptyDUG name

-- | The algorithm used here is stateful, and so we perform
-- the transformations of the current gen-state imperatively in the
-- state monad.
type GenState r = State GenSt r
