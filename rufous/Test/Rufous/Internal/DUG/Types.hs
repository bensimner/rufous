{-# LANGUAGE TemplateHaskell #-}
module Test.Rufous.Internal.DUG.Types where

import Control.Lens

import Data.Dynamic
import Data.List (intercalate)

import qualified Data.Map as M
import qualified Test.Rufous.Signature as S
import qualified Test.Rufous.Profile as P

type DUGArg = S.Arg Int Int ()
data Node =
   Node
      { _nodeId :: Int  -- Invariant: (DUG^.operations ! i)^.nodeId == i
      , _operation :: S.Operation
      , _args :: [DUGArg]  -- Version (-1) => undefined
      , _shadow :: Dynamic -- the shadow
      , _dyn :: Maybe Dynamic    -- the actual underlying value
      }
makeLenses ''Node

instance Show Node where
   show n = "(" ++ intercalate " " ["Node", show $ n^.nodeId, show $ n^.operation^.S.opName, as] ++ ")"
      where as = show [i | S.Version i <- n^.args]

-- | Info related to the way this DUG was generated.
data DUGGenerationInfo =
   GInfo
      { _idx :: Integer  -- index 0, 1, 2, ... of generated DUG
      , _targetProfile :: P.Profile -- the generated or supplied target profile
      }
   deriving (Show)
makeLenses ''DUGGenerationInfo

data DUG =
   DUG
      { _name :: String
      , _operations :: M.Map Int Node
      , _ginfo :: Maybe DUGGenerationInfo
      }
   deriving (Show)
makeLenses ''DUG

nodeAt :: DUG -> Int -> Node
nodeAt d i = (d^.operations) M.! i

nodes :: DUG -> [Node]
nodes d = d ^. operations & M.elems

edges :: DUG -> [(Int, Int, Int)]
edges d = [(v, i, n ^. nodeId) | n <- nodes d, (i, S.Version v) <- zip [0..] (n^.args)]

edgesTo :: DUG -> Node -> [Int]
edgesTo _ n = [k | S.Version k <- n ^. args]

edgesFrom :: DUG -> Node -> [Int]
edgesFrom d i = [k | (j, _, k) <- edges d, j == i^.nodeId]

-- | Create a new empty DUG with a given name
emptyDUG :: String -> DUG
emptyDUG dugName = DUG dugName M.empty Nothing

nextId :: DUG -> Int
nextId = length . nodes

-- | Create and insert a new Node
pushNew :: S.Operation -> [DUGArg] -> Dynamic -> DUG -> DUG
pushNew o dargs dyn dug =
      dug & operations . at newId ?~ n
   where
      newId = nextId dug
      n = Node newId o dargs dyn Nothing

size :: DUG -> Int
size d = M.size $ d^.operations
