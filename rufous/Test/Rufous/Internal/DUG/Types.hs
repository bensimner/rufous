{-# LANGUAGE TemplateHaskell #-}
module Test.Rufous.Internal.DUG.Types where

import Control.Lens

import Data.Dynamic
import Data.List (intercalate)

import qualified Data.Map as M
import qualified Test.Rufous.Signature as S

type DUGArg = S.Arg Int Int Int Bool
data Node =
   Node
      { _nodeId :: Int  -- Invariant: (DUG^.operations ! i)^.nodeId == i
      , _operation :: S.Operation
      , _args :: [DUGArg]
      , _shadow :: Dynamic
      }
makeLenses ''Node

instance Show Node where
   show n = "(" ++ intercalate " " ["Node", show $ n^.nodeId, show $ n^.operation^.S.opName, as] ++ ")"
      where as = show [i | S.Version i <- n^.args]

data DUG =
   DUG
      { _name :: String
      , _operations :: M.Map Int Node
      }
   deriving (Show)
makeLenses ''DUG

nodes :: DUG -> [Node]
nodes d = d ^. operations & M.elems

edges :: DUG -> [(Int, Int)]
edges d = [(v, n ^. nodeId) | n <- nodes d, S.Version v <- n^.args]

edgesTo :: DUG -> Node -> [Int]
edgesTo d n = [k | S.Version k <- n ^. args]

edgesFrom :: DUG -> Node -> [Int]
edgesFrom d i = [k | (j, k) <- edges d, j == i^.nodeId]

-- | Create a new empty DUG with a given name
emptyDUG :: String -> DUG
emptyDUG name = DUG name M.empty

nextId :: DUG -> Int
nextId = length . nodes

-- | Create and insert a new Node
pushNew :: S.Operation -> [DUGArg] -> Dynamic -> DUG -> DUG
pushNew op args shadow d =
      d & operations . at newId ?~ n
   where
      newId = nextId d
      n = Node newId op args shadow

size :: DUG -> Int
size d = M.size $ d^.operations
