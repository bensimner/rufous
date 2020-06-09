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

newtype ShadowList t = S Int
   deriving (Show)

instance ListADT ShadowList where
   listcons _ (S i) = S (i + 1)
   listempty = S 0
   listhead (S 0) = guardFailed
   listhead (S _) = shadowUndefined

data FakeList a = EF | F a (FakeList a)
   deriving (Show)

instance ListADT FakeList where
   listcons y f = F y f
   listempty = EF
   listhead EF = undefined
   listhead (F x _) = x

makeADTSignature ''ListADT

main :: IO ()
main = mainWith args{signature=_ListADT, verbose=True}