{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Main where

import Debug.Trace
import Control.Concurrent.MVar

import Test.Rufous
import Test.Rufous.Internal.DUG.DotPrinter (printDUG)
import Test.Rufous.Extract
import Test.Rufous.Internal.Signature.Types

class ListADT t where
   listcons :: a -> t a -> t a
   listempty :: t a
   listhead :: t a -> a

instance ListADT [] where
   listcons = (:)
   listempty = []
   listhead = head

-- makeADTSignature ''ListADT
instance ListADT (Test.Rufous.Extract.WrappedADT []) where
   listcons x0 x1
     = let curId_a7FH = Test.Rufous.Extract._get_id "listcons"
       in traceShow ("listcons_get", curId_a7FH)
         ((Test.Rufous.Extract._log_operation curId_a7FH) "listcons")
           ((listcons
               ((((Test.Rufous.Extract.nonversion curId_a7FH) 0)
                   (Test.Rufous.Internal.Signature.Types.NonVersion
                      (Test.Rufous.Internal.Signature.Types.VersionParam x0)))
                  x0))
              (((Test.Rufous.Extract.unwrap curId_a7FH) 1) x1))
   listempty
     = let curId_a7FI = Test.Rufous.Extract._get_id "listempty"
       in traceShow ("listempty_get", curId_a7FI)
         ((Test.Rufous.Extract._log_operation curId_a7FI) "listempty")
           listempty
   listhead x0
     = let curId_a7FJ = Test.Rufous.Extract._get_id "listhead"
       in ((Test.Rufous.Extract._log_observer curId_a7FJ) "listhead")
           (listhead (((Test.Rufous.Extract.unwrap curId_a7FJ) 0) x0))

program :: IO ()
program = print $ listhead (listcons 1 (listempty :: Extracted []))

main :: IO ()
main = do
   (_, dug) <- extract _ListADT program
   print dug
   printDUG "output/" dug
   -- let st = emptyExtractorState
   -- print $ "putting..."
   -- putMVar state st
   -- print (_get_id(), _get_id())
