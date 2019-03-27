{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Main where

import Test.Rufous

class ListADT t where
   listcons :: a -> t a -> t a
   listempty :: t a
   listhead :: t a -> a

instance ListADT [] where
   listcons = (:)
   listempty = []
   listhead = head

makeADTSignature ''ListADT

program :: IO ()
program = print $ listhead v0
   where v0 = listcons 1 v1
         v1 = listcons 2 v2
         v2 = listcons 3 v4
         v3 = listcons 4 v4
         v4 = listempty :: Extracted []

main :: IO ()
main = do
   (_, dug) <- extract _ListADT program
   print ()
