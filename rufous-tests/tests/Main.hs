{-# LANGUAGE BangPatterns #-}
module Main where

import Test.Rufous
import Test.Rufous.Internal.DUG.HsPrinter (sprintDUG)

import qualified ADTs.Queue.Definition as QDef
import qualified ADTs.Queue.Generate as QGen

import qualified ADTs.List.Definition as LDef
import qualified ADTs.List.Generate as LGen

mainObsQ :: QDef.Queuey q => q Int -> IO ()
mainObsQ v = do
   (_, !obs) <- QGen.generateCalls v 102
   return ()

mainObsL :: LDef.Listy l => l Int -> IO ()
mainObsL v = do
   (_, !obs) <- LGen.generateCalls v 102
   return ()

main = do
   putStrLn "Queue:"
   dug <- extract QDef._Queuey (mainObsQ (QDef.qempty :: Extracted []))
   putStrLn "Extracted DUG:"
   print dug
   putStrLn "Extracted Source:"
   putStrLn $ sprintDUG dug

   putStrLn (replicate 80 '-')
   putStrLn "List:"
   dug <- extract LDef._Listy (mainObsL (LDef.lempty :: Extracted []))
   putStrLn "Extracted DUG:"
   print dug
   putStrLn "Extracted Source:"
   putStrLn $ sprintDUG dug
