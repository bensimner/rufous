{-# LANGUAGE TemplateHaskell #-}
module Test.Rufous.Internal.DUG.Types where

import Control.Lens

import Data.Dynamic
import Data.List (intercalate)

import qualified Data.Map as M
import qualified Test.Rufous.Signature as S
import qualified Test.Rufous.Profile as P

import qualified Test.Rufous.Internal.Generate.MSet as MSt

type DUGArg = S.Arg Int Int ()
data Node =
   Node
      { _nodeId :: Int  -- Invariant: (DUG^.operations ! i)^.nodeId == i
      , _operation :: S.Operation
      , _args :: [DUGArg]  -- Version (-1) => undefined
      , _shadow :: Maybe Dynamic -- the shadow
      , _dyn :: Maybe Dynamic    -- the actual underlying value
      }
makeLenses ''Node

instance Show Node where
   show n = "(" ++ intercalate " " ["Node", show $ n^.nodeId, show $ n^.operation^.S.opName, as] ++ ")"
      where as = show [i | S.Version i <- n^.args]

-- | Info related to the way this DUG was generated.
data DUGGenerationInfo =
   GInfo
      { _idx :: Int  -- index 0, 1, 2, ... of generated DUG
      , _targetProfile :: P.Profile -- the generated or supplied target profile
      }
   deriving (Show)
makeLenses ''DUGGenerationInfo

data DUG =
   DUG
      { _name :: String
      , _operations :: M.Map Int Node
      , _reverseArgs :: M.Map Int (MSt.MSet Int)
      , _ginfo :: Maybe DUGGenerationInfo
      , _dugSize :: Int
      }
   deriving (Show)
makeLenses ''DUG

nodeAt :: Int -> DUG -> Node
nodeAt i d = (d^.operations) M.! i

nodes :: DUG -> [Node]
nodes d = d ^. operations & M.elems

edges :: DUG -> [(Int, Int, Int)]
edges d = [(v, i, n ^. nodeId) | n <- nodes d, (i, S.Version v) <- zip [0..] (n^.args)]

edgesTo :: DUG -> Node -> [Int]
edgesTo _ n = [k | S.Version k <- n ^. args]

edgesFrom :: DUG -> Node -> [Int]
edgesFrom d n = MSt.toList $ (d^.reverseArgs) M.! (n^.nodeId)

-- | Create a new empty DUG with a given name
emptyDUG :: String -> DUG
emptyDUG dugName = DUG dugName M.empty M.empty Nothing 0

nextId :: DUG -> Int
nextId d = 1 + M.size (d^.operations)

-- | Create and insert a new Node
pushNew :: S.Operation -> [DUGArg] -> Maybe Dynamic -> DUG -> DUG
pushNew o dargs shadowDyn dug =
      dug & operations . at newId ?~ n
          & reverseArgs %~ pushRevArgs newId dargs
          & reverseArgs %~ M.insert newId MSt.empty
          & dugSize .~ newId
   where
      newId = nextId dug
      n = Node newId o dargs shadowDyn Nothing

pushRevArgs :: Int -> [DUGArg] -> M.Map Int (MSt.MSet Int) -> M.Map Int (MSt.MSet Int)
pushRevArgs nId dargs m = go dargs m
   where
      go (S.Version v : dargs') m' =
         M.update (Just . MSt.insert nId) v (go dargs' m')
      go (_ : dargs') m' = go dargs' m'
      go [] m' = m'

size :: DUG -> Int
size d = M.size $ d^.operations

observers :: DUG -> [Node]
observers d = filter isObserver $ d ^. operations ^.. traverse

mutators :: DUG -> [Node]
mutators d = filter isMutator $ d ^. operations ^.. traverse

generators :: DUG -> [Node]
generators d = filter isGenerator $ d ^. operations ^.. traverse

isObserver :: Node -> Bool
isObserver n = n^.operation^.S.opCategory == S.Observer

isMutator :: Node -> Bool
isMutator n = n^.operation^.S.opCategory == S.Mutator

isGenerator :: Node -> Bool
isGenerator n = n^.operation^.S.opCategory == S.Generator