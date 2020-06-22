module Test.Rufous.Internal.Generate.LivingSet where

import Control.Lens

import qualified Data.Map as M
import qualified Data.Set as St

import Prelude hiding (concat)
import Data.Foldable (foldl')

import qualified Test.Rufous.Signature as S

-- | The LivingSet is the frontier
-- containing the set of living nodes.
data LivingSet =
    Living
        { total :: St.Set Int

        -- | unused is a heuristic set of nodes
        -- that probably aren't used in buffered operations
        -- and so should be preferentially picked
        , unused :: St.Set Int

        -- | whether a particular node is considered alive
        -- for a particular operation.
        -- e.g. a node may be considered dead by `head`
        -- but not by `snoc` to help prevent guard failures
        -- perOpTotal M.! s  <=  total
        , perOpTotal :: M.Map String (St.Set Int)

        -- | per operation heuristics
        -- perOpUnused M.! s  <=  unused
        , perOpUnused :: M.Map String (St.Set Int)

        -- | a set of all the nodes that have out-degree 0
        -- in the _committed_ dug
        -- note that these nodes might already be in 'use'
        -- by buffered operations
        -- infants <= total
        , infants :: St.Set Int
        , infantUnused :: St.Set Int
        , infantPerOpTotal :: M.Map String (St.Set Int)
        , infantPerOpUnused :: M.Map String (St.Set Int)

        -- | ordered frontier
        -- on a commit then remove the oldest from this list
        , age :: [Int]
        }

size :: LivingSet -> Int
size = St.size . total

imageUnused :: String -> LivingSet -> St.Set Int
imageUnused o liv = (perOpUnused liv) M.! o

imageUnusedInf :: String -> LivingSet -> St.Set Int
imageUnusedInf o liv = (infantPerOpUnused liv) M.! o

image :: String -> LivingSet -> St.Set Int
image o liv = (infantPerOpTotal liv) M.! o

imageInf :: String -> LivingSet -> St.Set Int
imageInf o liv = (infantPerOpTotal liv) M.! o

useInf :: Int -> LivingSet -> LivingSet
useInf n (Living tot totUnused op opUnused inf infUnused infOp infOpUnused age) = Living tot' totUnused' op' opUnused' inf' infUnused' infOp' infOpUnused' age
    where
        tot' = tot
        totUnused' = St.delete n totUnused
        op' = op
        opUnused' = M.map (St.delete n) opUnused
        inf' = inf
        infUnused' = St.delete n infUnused
        infOp' = infOp
        infOpUnused' = M.map (St.delete n) infOpUnused

usePers :: Int -> LivingSet -> LivingSet
usePers n (Living tot totUnused op opUnused inf infUnused infOp infOpUnused age) = Living tot' totUnused' op' opUnused' inf infUnused infOp infOpUnused age
    where
        tot' = tot
        totUnused' = St.delete n totUnused
        op' = op
        opUnused' = M.map (St.delete n) opUnused

timeToDie :: Maybe Int -> Bool
timeToDie Nothing = True
timeToDie (Just n) = n < 100

unuse :: Int -> LivingSet -> LivingSet
unuse n (Living tot totUnused op opUnused inf infUnused infOp infOpUnused age) = Living tot' totUnused' op' opUnused' inf' infUnused' infOp' infOpUnused' age'
    where
        tot' = tot
        totUnused' = St.insert n totUnused
        op' = op
        opUnused' = M.map (St.insert n) opUnused
        -- we're committing to the DUG so it's no longer an infant.
        inf' = St.delete n inf
        infUnused' = St.delete n infUnused
        infOp' = M.map (St.delete n) infOp
        infOpUnused' = M.map (St.delete n) infOpUnused
        age' = age

member :: String -> Int -> LivingSet -> Bool
member o n liv = n `St.member` (perOpTotal liv M.! o)

memberAll :: Int -> LivingSet -> Bool
memberAll s liv = s `St.member` (total liv)

empty :: S.Signature -> LivingSet
empty s = Living empSet empSet empMap empMap empSet empSet empMap empMap []
    where ops = M.keys $ s^.S.operations
          empMap = M.fromList [(o, St.empty) | o <- ops]
          empSet = St.empty

insert :: String -> Int -> LivingSet -> LivingSet
insert o n (Living tot totUnused op opUnused inf infUnused infOp infOpUnused age) = Living tot' totUnused' op' opUnused' inf' infUnused' infOp' infOpUnused' age
    where
        tot' = St.insert n tot
        totUnused' = St.insert n totUnused
        op' = M.update (Just . St.insert n) o op
        opUnused' = M.update (Just . St.insert n) o opUnused
        inf' = St.insert n inf
        infUnused' = St.insert n infUnused
        infOp' = M.update (Just . St.insert n) o infOp
        infOpUnused' = M.update (Just . St.insert n) o infOpUnused


-- | when removing a node from the frontier but only for a particular operation
deleteOp :: String -> Int -> LivingSet -> LivingSet
deleteOp o n (Living tot totUnused op opUnused inf infUnused infOp infOpUnused age) = Living tot' totUnused' op' opUnused' inf' infUnused' infOp' infOpUnused' age
    where
        tot' = tot
        totUnused' = St.insert n totUnused
        op' = M.update (Just . St.delete n) o op
        opUnused' = M.update (Just . St.delete n) o opUnused
        inf' = inf
        infUnused' = infUnused
        infOp' = M.update (Just . St.delete n) o infOp
        infOpUnused' = M.update (Just . St.delete n) o infOpUnused

concat :: LivingSet -> St.Set Int
concat liv = total liv

addNewInfant :: S.Signature -> Int -> LivingSet -> LivingSet
addNewInfant s n liv =
        if size liv'' > 100 then
            removeOldest liv''
        else
            liv''
    where ops = M.keys $ s^.S.operations
          liv' = foldl' (\v o -> insert o n v) liv ops
          liv'' = liv'{age=n:age liv'}


removeOldest :: LivingSet -> LivingSet
removeOldest liv = liv'{age=age'}
    where part [x] ys = (x, ys)
          part (x:xs) ys = part (xs) (x:ys)
          part [] _ = error "Rufous: internal: removeOldest: empty frontier?"
          (oldest, age') = part (age liv) []
          liv' = removeFromFrontier oldest liv


-- | delete a node from the frontier entirely
removeFromFrontier :: Int -> LivingSet -> LivingSet
removeFromFrontier n (Living tot totUnused op opUnused inf infUnused infOp infOpUnused age) = Living tot' totUnused' op' opUnused' inf' infUnused' infOp' infOpUnused' age
    where
        tot' = St.delete n tot
        totUnused' = St.delete n totUnused
        op' = M.map (St.delete n) op
        opUnused' = M.map (St.delete n) opUnused
        inf' = St.delete n inf
        infUnused' = St.delete n infUnused
        infOp' = M.map (St.delete n) infOp
        infOpUnused' = M.map (St.delete n) infOpUnused