module Test.Rufous.Internal.DUG.ProfileExtractor where

import Control.Lens
import qualified Data.Map as M

import qualified Test.Rufous.Profile as P
import qualified Test.Rufous.Signature as S

import Test.Rufous.Internal.DUG.Types


extractProfile :: S.Signature -> DUG -> P.Profile
extractProfile s d = p
   where ws = weights d
         ps = persistents s d
         m = mortality d
         sz = length (nodes d)
         p = blankProfile s
               & P.operationWeights %~ M.unionWith (+) ws
               & P.persistentApplicationWeights %~ M.unionWith (+) ps
               & P.mortality .~ m
               & P.size .~ sz


blankProfile :: S.Signature -> P.Profile
blankProfile s = P.Profile ps ps m sz
   where ps = M.fromList [(k, 0) | k <- M.keys (s ^. S.operations)]
         m = 0
         sz = 0

mortality :: DUG -> Float
mortality d = living `guardedDiv` total
   where total  = fromIntegral $ length (nodes d)
         living = fromIntegral $ length [n | n <- nodes d, length (edgesFrom d n) > 0]

weights :: DUG -> M.Map String Float
weights d = normMap $ foldl (\m n -> M.insertWith (\_ v -> v+1) (n^.operation^.S.opName) 0 m) M.empty (nodes d)

normMap :: M.Map String Float -> M.Map String Float
normMap m = M.map (`guardedDiv` tot) m
   where tot = sum (M.elems m)

persistents :: S.Signature -> DUG -> M.Map String Float
persistents s d = M.fromList [(o, persistent o) | o <- opNames]
   where opNames = M.keys $ s^.S.operations
         allNodes = [(n, isPersistent n d) | n <- nodes d]
         allNodesOp o = [n | n <- nodes d, n^.operation^.S.opName == o]
         persistent o = realLen [() | (n,True) <- allNodes, n^.operation^.S.opName == o] `guardedDiv` (realLen (allNodesOp o))
         realLen = fromIntegral . length

isPersistent :: Node -> DUG -> Bool
isPersistent node d =
   if null mutators
      then True
      else n > earliest
   where n = node^.nodeId
         indexes = edgesFrom d node
         mutators = [i | i <- indexes, isMutator (nodeAt i d)]
         earliest = minimum mutators


-- | for each note, return a map which says
-- how many of a particular operation were performed on it.
kinded :: Num a => DUG -> Node -> String -> a
kinded d node opName =
   sum [1 | n' <- edgesFrom d node, (nodeAt n' d)^.operation^.S.opName == opName]

-- Sometimes NaN appears for DUGs with no operations of one weight or no persistent
-- applications, and these must be rounded to 0.
guardedDiv :: RealFloat a => a -> a -> a
guardedDiv a b =
   if isNaN (a/b) then
      0
   else
      a/b

edgesFromDUG :: DUG -> M.Map Int [Int]
edgesFromDUG d = M.map (\n -> edgesFrom d n) (d ^. operations)
