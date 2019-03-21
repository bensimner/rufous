{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Test.Rufous.Internal.Generate.Types where

import Control.Monad.State
import Control.Lens
import System.Random (StdGen, mkStdGen)

import qualified Data.Sequence as Sq
import qualified Data.Set as St
import qualified Data.Map as M

import qualified Test.Rufous.Options as Opt
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
   | Concrete D.DUGArg (Maybe PersistenceType)
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

data DebugInfo =
   Dbg
      { _failedGuards :: Int             -- the number of dtimes BufferedOperation's failed their guards
      , _diedOfOldAge :: Int             -- the number of BufferedOperation's who died before leaving the buffer
      , _deflateSteps :: Int             -- the total number of deflation steps
      , _flatDeflateSteps :: Int         -- the number of deflate steps which failed to deflate anything.
      , _noLivingNodes :: Int            -- the number of times a BuffereOperation couldn't be satisfied because there were no living nodes
      , _inflatedOps :: M.Map String Int -- the number of times a BuffereOperation couldn't be satisfied because there were no living nodes
      , _deadNodes :: Int                -- a count of the number of nodes that have died so far
      , _dbgTrace :: [String]
      }
   deriving (Show)
makeLenses ''DebugInfo

-- | During generation there is a lot of state that is kept:
--    - the current partially built DUG
--    - the buffer of uncommitted operations
--    - the set of not-dead nodes
-- Along with any options for the creation
data GenSt =
   GenSt
      { _opt :: Opt.RufousOptions
      , _sig :: S.Signature
      , _profile :: P.Profile
      , _buffer :: Sq.Seq BufferedOperation
      , _dug :: D.DUG
      , _living :: St.Set Int
      , _mutators :: NodeBucket
      , _observers :: NodeBucket
      , _nodeCounts :: M.Map Int Int
      , _gen :: StdGen
      , _dbg :: DebugInfo
      }
makeLenses ''GenSt

-- | An empty node bucket has empty infant/persistent sets
emptyNodeBucket :: NodeBucket
emptyNodeBucket = NodeBucket MSt.empty MSt.empty

-- | Create an empty gen state
-- TODO: better seed...
emptyGenSt :: Opt.RufousOptions -> S.Signature -> P.Profile -> String -> GenSt
emptyGenSt o s p name = GenSt o s p Sq.empty d St.empty emptyNodeBucket emptyNodeBucket nc (mkStdGen 0) debug
   where d = D.emptyDUG name
         debug = Dbg 0 0 0 0 0 M.empty 0 []
         nc = M.empty

-- | The algorithm used here is stateful, and so we perform
-- the transformations of the current gen-state imperatively in the
-- state monad.
type GenState r = State GenSt r

-- TODO: do I neeed this generic type signature or can it just be (Int -> Int) ?
debugIf :: Bool -> Lens' DebugInfo a -> (a -> a) -> GenState ()
debugIf b m f =
   if b then
      updateDbg m f
   else
      return ()

updateDbg :: Lens' DebugInfo a -> (a -> a) -> GenState ()
updateDbg m f = do
   opts <- use opt
   if Opt.debug opts then
      dbg . m %= f
   else return ()

debugTrace :: String -> GenState ()
debugTrace msg = do
   opts <- use opt
   if Opt.debug opts then
      dbg . dbgTrace %= (msg:)
   else return ()
