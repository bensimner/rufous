module Test.Rufous.Internal.Generate.LivingSet where

import Control.Lens

import qualified Data.Map as M
import qualified Data.Set as St

import Prelude hiding (concat)
import Data.Foldable (foldl')

import qualified Test.Rufous.Signature as S

type LivingSet = M.Map String (St.Set Int)

size :: LivingSet -> Int
size = St.size . concat

image :: String -> LivingSet -> St.Set Int
image op liv = liv M.! op

member :: String -> Int -> LivingSet -> Bool
member op n liv = n `St.member` (liv M.! op)

memberAll :: Int -> LivingSet -> Bool
memberAll s liv = s `St.member` (concat liv)

empty :: S.Signature -> LivingSet
empty s = M.fromList [(op, St.empty) | op <- ops]
    where ops = M.keys $ s^.S.operations

insert :: String -> Int -> LivingSet -> LivingSet
insert op n = M.update (Just . St.insert n) op

delete :: String -> Int -> LivingSet -> LivingSet
delete op n = M.update (Just . St.delete n) op

concat :: LivingSet -> St.Set Int
concat liv = foldl' St.union St.empty (M.elems liv)

insertAll :: S.Signature -> Int -> LivingSet -> LivingSet
insertAll s n liv = foldl' (\v o -> insert o n v) liv ops
    where ops = M.keys $ s^.S.operations

deleteAll :: S.Signature -> Int -> LivingSet -> LivingSet
deleteAll s n liv = foldl' (\v o -> delete o n v) liv ops
    where ops = M.keys $ s^.S.operations