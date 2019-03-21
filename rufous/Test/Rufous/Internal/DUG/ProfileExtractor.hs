module Test.Rufous.Internal.DUG.ProfileExtractor where

import Control.Lens
import qualified Data.Maybe as My

import qualified Data.Map as M

import qualified Test.Rufous.Profile as P
import qualified Test.Rufous.Signature as S

import Test.Rufous.Internal.DUG.Types


extractProfile :: S.Signature -> DUG -> P.Profile
extractProfile s d = p
   where ws = weights d
         ps = persistents d
         m = mortality d
         p = blankProfile s
               & P.operationWeights %~ M.unionWith (+) ws
               & P.persistentApplicationWeights %~ M.unionWith (+) ps
               & P.mortality .~ m


blankProfile :: S.Signature -> P.Profile
blankProfile s = P.Profile ps ps m
   where ps = M.fromList [(k, 0) | k <- M.keys (s ^. S.operations)]
         m = 0

mortality :: DUG -> Float
mortality d = living `guardedDiv` total
   where total  = fromIntegral $ length (nodes d)
         living = fromIntegral $ length [n | n <- nodes d, length (edgesFrom d n) > 0]

weights :: DUG -> M.Map String Float
weights d = normMap $ foldl (\m n -> M.insertWith (\_ v -> v+1) (n^.operation^.S.opName) 0 m) M.empty (nodes d)

normMap :: M.Map String Float -> M.Map String Float
normMap m = M.map (`guardedDiv` tot) m
   where tot = sum (M.elems m)

persistents :: DUG -> M.Map String Float
persistents d = M.fromList [(o, persistent o) | o <- opNames]
   where opNames = [n^.operation^.S.opName | n <- nodes d]
         allNodes = [kinded d n | n <- nodes d]
         allNodesOp o = allNodes <*> [o]
         persistent o = sum [max (x - 1) 0 | x <- allNodesOp o] `guardedDiv` (sum (allNodesOp o))

-- | for each note, return a map which says
-- how many of a particular operation were performed on it.
kinded :: Num a => DUG -> Node -> String -> a
kinded d node opName =
   sum [1 | n' <- edgesFrom d node, (nodeAt d n')^.operation^.S.opName == opName]

-- Sometimes NaN appears for DUGs with no operations of one weight or no persistent
-- applications, and these must be rounded to 0.
guardedDiv a b =
   if isNaN (a/b) then
      0
   else
      a/b

edgesFromDUG :: DUG -> M.Map Int [Int]
edgesFromDUG d = M.map (\n -> edgesFrom d n) (d ^. operations)
