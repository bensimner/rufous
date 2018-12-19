module Test.Rufous.Internal.Generate.MSet where

import qualified Data.Map as M
import qualified Data.Set as St

type MSet a = M.Map a Int

member :: Ord a => a -> MSet a -> Bool
member a set =
   case M.lookup a set of
      Nothing -> False
      Just 0 -> False
      Just _ -> True

insert :: Ord a => a -> MSet a -> MSet a
insert a set = M.insertWith (\_ v -> v + 1) a 1 set

delete :: Ord a => a -> MSet a -> MSet a
delete a set = M.update (\v -> del $ v - 1) a set
   where del 0 = Nothing
         del k = Just k

empty :: MSet a
empty = M.empty

toList :: Ord a => MSet a -> [a]
toList mset = [k | (k, v) <- M.toList mset, v > 0]

toSet :: Ord a => MSet a -> St.Set a
toSet = St.fromList . toList
