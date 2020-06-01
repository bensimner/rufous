{-# LANGUAGE TemplateHaskell, TypeFamilies, FlexibleInstances #-}
module Main where

import Test.Rufous

import qualified Data.Map as DM
--import qualified Data.HMap as HM
--import qualified Useful.Dictionary as UD

class MapADT m where
   type Key m
   mapempty :: m v
   mapinsert :: Key m -> v -> m v -> m v
   --maplookup :: Key m -> m v -> Maybe v

data ShadowMap v = Null

instance MapADT ShadowMap where
   type Key ShadowMap = Int
   mapempty = Null
   mapinsert _ _ _ = Null
   --maplookup = shadowUndefined

instance Ord k => MapADT (DM.Map k) where
   type Key (DM.Map k) = k
   mapempty = DM.empty
   mapinsert = DM.insert
   --maplookup = DM.lookup

makeADTSignature ''MapADT

main :: IO ()
main = mainWith args{signature=_MapADT, verbose=True}