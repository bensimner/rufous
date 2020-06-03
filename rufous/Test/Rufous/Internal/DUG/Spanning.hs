module Test.Rufous.Internal.DUG.Spanning where

import Debug.Trace

import Control.Lens ((^.), (^..))

import qualified Data.Set as St
import qualified Data.Map as M
import qualified Data.List as L

import qualified Test.Rufous.Signature as S

import Test.Rufous.Internal.DUG.Types

-- | finds the best directed spanning tree through the DUG such that
-- a < b iff a does not require b's node
-- this requires the DUG to be acyclic.
--
orderedMSTFlatten :: DUG -> [Node]
orderedMSTFlatten d = L.sortOn (\n -> n^.nodeId) (M.elems (d^.operations))