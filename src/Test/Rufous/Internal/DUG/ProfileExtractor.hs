module Test.Rufous.Internal.DUG.ProfileExtractor where

import Control.Lens
import qualified Data.Maybe as My

import qualified Data.Map as M

import qualified Test.Rufous.Profile as P
import qualified Test.Rufous.Signature as S

import Test.Rufous.Internal.DUG.Types


extractProfile :: S.Signature -> DUG -> P.Profile
extractProfile s d = P.Profile ws ps m
   where ws = weights s d
         ps = persistents s d
         m = mortality d


mortality :: DUG -> Float
mortality d = (fromIntegral living) / (fromIntegral total)
   where total  = length (nodes d)
         living = length [n | n <- nodes d, length (edgesFrom d n) > 0]

weights :: S.Signature -> DUG -> M.Map String Float
weights s d = M.map (\v -> (fromIntegral v) / (fromIntegral total)) counts
   where initial = M.fromList [(k, 0) | k <- M.keys (s^.S.operations)]
         counts = foldl (\m n -> M.insertWith (\_ v -> v+1) (n^.operation^.S.opName) 0 m) M.empty (nodes d)
         total = length (nodes d)


persistents :: S.Signature -> DUG -> M.Map String Float
persistents s d = M.map (\v -> (fromIntegral v) / (fromIntegral total)) counts
   where initial = M.fromList [(k, 0) | k <- M.keys (s^.S.operations)]
         counts = foldl (\m n -> M.insertWith (\_ v -> v+1) (n^.operation^.S.opName) 0 m) M.empty (nodes d)
         total = length (nodes d)
         appkinds = M.map (map (My.fromJust . kind)) (edgesFromDUG d)
         kind :: Int -> Maybe S.OperationCategory
         kind j = d^.operations^.at j ^? _Just . operation . S.opCategory

edgesFromDUG :: DUG -> M.Map Int [Int]
edgesFromDUG d = M.map (\n -> edgesFrom d n) (d ^. operations)
