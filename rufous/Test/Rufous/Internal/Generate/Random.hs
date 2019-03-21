module Test.Rufous.Internal.Generate.Random where

import System.Random
import qualified Data.Set as St
import qualified Data.Map as M
import qualified Data.List as L

import Control.Monad.State
import Control.Lens

import Test.Rufous.Internal.Generate.Types

genRandomR :: Random a => (a, a) -> GenState a
genRandomR r = do
   g <- use gen
   let (res, g') = randomR r g
   gen .= g'
   return res

genUniform :: [a] -> GenState a
genUniform opts = do
      i <- genRandomR (0, n - 1)
      return (opts !! i)
   where
      n = length opts

genUniformSet :: St.Set a -> GenState a
genUniformSet s = do
   i <- genRandomR (0, St.size s - 1)
   return $ St.elemAt i s

genBool :: Float -> GenState Bool
genBool pTrue = do
   p <- genRandomR (0, 1)
   if p < pTrue then
      return True
   else
      return False

genAccordingTo :: Ord k => M.Map k Float -> M.Map k v -> GenState v
genAccordingTo ps vs = do
      p <- genRandomR (0, 1)
      return $ go (accordingToSortByValue ps) p
   where go ((x,v):xs) p | p <= v = vs M.! x
         go ((x,v):xs) p | p > v = go xs (p - v)

accordingToSortByValue :: M.Map k Float -> [(k, Float)]
accordingToSortByValue vs = L.sortOn snd (M.toList vs)
