module Main where

import Test.Rufous

import qualified ADTs.Queue.Definition as QDef
import qualified ADTs.Queue.Generate as QGen

main = do
   ((_, obs), dug) <- extract QDef._Queuey (QGen.generateCalls (QDef.qempty :: Extracted []) 102)
   print obs
   print dug
