{-# LANGUAGE TemplateHaskell, BangPatterns, TypeFamilies, FlexibleInstances #-}
module Main where

import Test.Rufous

import qualified Data.Map as DM
--import qualified Data.HMap as HM
--import qualified Useful.Dictionary as UD

class MapADT m where
   type Key m
   mapempty :: m v
   mapinsert :: Key m -> v -> m v -> m v
   maplookup :: Key m -> m v -> Maybe v

forceMaplookup :: Maybe Int -> ()
forceMaplookup Nothing = ()
forceMaplookup (Just !_) = ()

data ShadowMap v = Null

instance MapADT ShadowMap where
   type Key ShadowMap = Int
   mapempty = Null
   mapinsert _ _ _ = Null
   maplookup = shadowUndefined

instance Ord k => MapADT (DM.Map k) where
   type Key (DM.Map k) = k
   mapempty = DM.empty
   mapinsert = DM.insert
   maplookup = DM.lookup

makeADTSignature ''MapADT

main :: IO ()
main = mainWith args{signature=_MapADT, verbose=True}

{- EXAMPLE OUTPUT

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 #tests | #versions | "mapempty" weight | "mapinsert" weight | "maplookup" weight | mortality |  pmf |  pof | Data.Map.Internal.Map
~~~~~~~~+~~~~~~~~~~~+~~~~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~~~~~+~~~~~~~~~~~+~~~~~~+~~~~~~+~~~~~~~~~~~~~~~~~~~~~~~
     16 |       911 |              0.55 |               0.29 |               0.16 |      0.44 | 0.00 | 0.01 |       0.000389234497s
      7 |      5605 |              0.62 |               0.27 |               0.11 |      0.36 | 0.02 | 0.03 |       0.001906515625s
     67 |        25 |              0.25 |               0.02 |               0.16 |      0.35 | 0.01 | 0.14 |        0.00002301484s
     10 |      2466 |              0.45 |               0.46 |               0.09 |      0.51 | 0.04 | 0.02 |       0.000532044921s
-}