{-# LANGUAGE BangPatterns #-}
module Main where

import Test.Rufous
import Test.Rufous.DUG (haskellSource)

import qualified ADTs.Queue.Definition as QDef
import qualified ADTs.Queue.Generate as QGen

import qualified ADTs.List.Definition as LDef
import qualified ADTs.List.Generate as LGen

discard :: a -> IO ()
discard x = x `seq` return ()

mainObsQ :: QDef.Queuey q => q Int -> IO ()
mainObsQ v = do
   (_, !obs) <- QGen.generateCalls v 102
   discard obs
   return ()

mainObsL :: LDef.Listy l => l Int -> IO ()
mainObsL v = do
   (_, !obs) <- LGen.generateCalls v 102
   discard obs
   return ()

run_dug :: String -> Signature -> IO () -> IO ()
run_dug dname sig prog = do
   putStrLn (dname ++ ":")
   dug <- extract sig prog
   putStrLn "Extracted DUG:"
   print dug
   putStrLn "Extracted Source:"
   putStrLn $ haskellSource dug

main :: IO ()
main = do
   run_dug "Queue" QDef._Queuey (mainObsQ (QDef.qempty :: Extracted []))
   putStrLn (replicate 80 '-')
   run_dug "List" LDef._Listy (mainObsL (LDef.lempty :: Extracted []))