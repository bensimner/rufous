{-# LANGUAGE TemplateHaskell, BangPatterns, TypeFamilies, FlexibleInstances #-}
module Main where

import Test.Rufous

import Data.Function (on)

import qualified Data.List as L

import qualified Data.Map
-- import qualified Data.HMap

import Data.Hashable
import qualified Data.HashMap

class MapADT m where
   type Key m
   mapempty :: m v
   mapinsert :: Key m -> v -> m v -> m v
   maplookup :: Key m -> m v -> Maybe v

-- not actually sure this is required
-- just whnf is enough to force enough
forceMaplookup :: Maybe Int -> ()
forceMaplookup Nothing = ()
forceMaplookup (Just !_) = ()

-- as always our hand-written simple list-based structure
-- to serve as a base and perhaps a specification for the Shadow
-- (for checking correctness of the other impls)
newtype ListDict k v = ListDict [(k,v)]
   deriving (Show)

instance Ord k => MapADT (ListDict k) where
   type Key (ListDict k) = k
   mapempty = ListDict []
   mapinsert k v (ListDict d) = ListDict $ (k,v) : L.deleteBy ((==) `on` fst) (k, undefined) d
   maplookup k (ListDict d) = snd <$> L.find ((== k) . fst) d

instance Ord k => MapADT (Data.Map.Map k) where
   type Key (Data.Map.Map k) = k
   mapempty = Data.Map.empty
   mapinsert = Data.Map.insert
   maplookup = Data.Map.lookup

instance (Hashable k, Ord k) => MapADT (Data.HashMap.Map k) where
   type Key (Data.HashMap.Map k) = k
   mapempty = Data.HashMap.empty
   mapinsert = Data.HashMap.insert
   maplookup = Data.HashMap.lookup

-- instance MapADT (HMap.HMap)
-- this has a Monadic API that does not match MapADT
-- so cannot apply Rufous to it
-- (sad face)

makeADTSignature ''MapADT

main :: IO ()
main = mainWith
         args
            { signature=_MapADT
            , averageDugSizes=[10000]
            , numberOfTests=50 -- number of DUGs to generate

            , info=True -- equivalent to saying verbosity=1
            , outputOptions=
               outputArgs
                  { dumpDUGs=False  -- set to True to create output/dugName.pdf graphviz output
                  , dumpDUGDetail=2 -- show shadow in the graphviz output
                  }
            }

{- EXAMPLE OUTPUT
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 #dugs |  size | "mapempty" weight | "mapinsert" weight | "maplookup" weight | mortality |  pmf |  pof | Main.ListDict | Data.Map.Internal.Map | Data.HashMap.Map
~~~~~~~+~~~~~~~+~~~~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~~~~~+~~~~~~~~~~~+~~~~~~+~~~~~~+~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~~~
     4 | 17233 |              0.20 |               0.21 |               0.59 |      0.21 | 0.62 | 1.00 |         0.23s |                 0.21s |            0.21s
    12 | 22450 |              0.20 |               0.32 |               0.48 |      0.22 | 0.64 | 1.00 |         0.55s |                 0.35s |            0.41s
    12 |  8899 |              0.24 |               0.41 |               0.35 |      0.20 | 0.75 | 1.00 |         0.19s |                 0.13s |            0.11s
     7 | 13064 |              0.37 |               0.26 |               0.36 |      0.22 | 0.75 | 1.00 |         0.15s |                 0.15s |            0.12s
    15 |  4097 |              0.26 |               0.13 |               0.60 |      0.20 | 0.81 | 1.00 |         0.11s |                 0.22s |            0.05s
-}