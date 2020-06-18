{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Main where

import Test.Rufous
import qualified Test.Rufous.Internal.DUG.DotPrinter as GraphViz
import qualified Test.Rufous.Internal.DUG.HsPrinter as SrcPrinter

class ListADT t where
   listcons :: a -> t a -> t a
   listempty :: t a
   listhead :: t a -> a
   listtail :: t a -> t a
   listnull :: t a -> Bool
   listappend :: t a -> t a -> t a

instance ListADT [] where
   listcons = (:)
   listempty = []
   listhead = head
   listtail = tail
   listnull = null
   listappend = (++)

makeADTSignature ''ListADT
makeExtractors ''ListADT

-- * Generated Program *
o0  = listnull v1
o2  = listnull v1
o3  = listnull v1
o4  = listnull v5
o6  = listnull v1
o7  = listnull v1
o8  = listnull v9
o10 = listnull v11
o12 = listnull v13
o15 = listnull v1
o16 = listnull v17
o18 = listnull v19
o21 = listnull v1
o22 = listnull v1
o23 = listnull v1
o24 = listnull v25
o26 = listnull v20
o27 = listnull v28
o29 = listnull v28
o30 = listnull v17
o31 = listnull v32
o34 = listnull v1
o35 = listnull v33
o36 = listnull v1
v1  = (listempty :: Extracted [])
v5  = listcons (undefined :: Extracted []) (undefined :: Extracted [])
v9  = listcons (undefined :: Extracted []) (undefined :: Extracted [])
v11 = listappend v1 v1
v13 = listappend v14 (undefined :: Extracted [])
v14 = listcons (undefined :: Extracted []) (undefined :: Extracted [])
v17 = listappend v14 (undefined :: Extracted [])
v19 = listappend v20 (undefined :: Extracted [])
v20 = listcons (undefined :: Extracted []) v14
v25 = listtail v20
v28 = listcons (undefined :: Extracted []) (undefined :: Extracted [])
v32 = listappend v1 v33
v33 = listappend v11 v14

program :: IO ()
program = mapM_ print [o0,o2,o3,o4,o6,o7,o8,o10,o12,o15,o16,o18,o21,o22,o23,o24,o26,o27,o29,o30,o31,o34,o35,o36]

-- expected extracted source
expected_src :: [Char]
expected_src = "\
   \o0 = listnull v1\n\
   \o2 = listnull v1\n\
   \o3 = listnull v1\n\
   \o4 = listnull v5\n\
   \o6 = listnull v1\n\
   \o7 = listnull v1\n\
   \o8 = listnull v9\n\
   \o10 = listnull v11\n\
   \o12 = listnull v13\n\
   \o15 = listnull v1\n\
   \o16 = listnull v17\n\
   \o18 = listnull v19\n\
   \o21 = listnull v1\n\
   \o22 = listnull v1\n\
   \o23 = listnull v1\n\
   \o24 = listnull v25\n\
   \o26 = listnull v20\n\
   \o27 = listnull v28\n\
   \o29 = listnull v28\n\
   \o30 = listnull v17\n\
   \o31 = listnull v32\n\
   \o34 = listnull v1\n\
   \o35 = listnull v33\n\
   \o36 = listnull v1\n\
   \v1 = listempty \n\
   \v5 = listcons undefined undefined\n\
   \v9 = listcons undefined undefined\n\
   \v11 = listappend v1 v1\n\
   \v13 = listappend v14 undefined\n\
   \v14 = listcons undefined undefined\n\
   \v17 = listappend v14 undefined\n\
   \v19 = listappend v20 undefined\n\
   \v20 = listcons undefined v14\n\
   \v25 = listtail v20\n\
   \v28 = listcons undefined undefined\n\
   \v32 = listappend v1 v33\n\
   \v33 = listappend v11 v14"

zipDefault :: b -> [b] -> [b] -> [(b, b)]
zipDefault a (x:xs) (y:ys) = (x,y) : zipDefault a xs ys
zipDefault a [] (y:ys) = (a,y) : zipDefault a [] ys
zipDefault a (x:xs) [] = (x,a) : zipDefault a xs []
zipDefault _ [] [] = []

main :: IO ()
main = do
   dug <- extract _ListADT program
   putStrLn "Extracted DUG:"
   print dug
   GraphViz.printDUG args "output/" dug
   putStrLn "Extracted Source:"
   let src = SrcPrinter.sprintDUG dug
   putStrLn src

   -- diff the output with the actual source
   -- this is a dumb line-by-line diff
   let diffs = [(l, k) | (l, k) <- zipDefault "-missing-" (lines src) (lines expected_src), l /= k]
   if not (null diffs) then do
      putStrLn "differences from original:"
      sequence_ $ do
         (l, k) <- diffs
         return $ putStrLn $ " ! " ++ show k ++ " vs " ++ show l
   else return ()
