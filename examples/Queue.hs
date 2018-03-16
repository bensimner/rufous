{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Main where

import Test.Rufous 
   ( makeADTSpec
   , runRufous
   , runRufousWithOptions
   , args
   , RufousOptions(..)
   , shadowUndefined
   , guardFailed )

import Test.Rufous.DUG as D
import Test.Rufous.Generate as G

data List a = Nil | Cons a (List a)
   deriving (Show)

class Queue q where
   snoc :: a -> q a -> q a
   empty :: q a 
   head' :: q a -> a

newtype ListQueue a = ListQueue [a]
   deriving (Show)

instance Queue ListQueue where
   snoc x (ListQueue xs) = ListQueue (xs ++ [x])
   empty     = ListQueue []
   head' (ListQueue xs) = head xs

instance Queue List where
   snoc x xs = Cons x xs
   empty = Nil
   head' (Cons x xs) = x
   head' Nil = undefined

data ShadowQueue x = ShadowQueue Int
   deriving (Show)

instance Queue ShadowQueue where
   snoc x (ShadowQueue xs) = ShadowQueue (xs + 1)
   empty     = ShadowQueue 0
   head' (ShadowQueue xs) | xs > 0  = shadowUndefined
   head' (ShadowQueue xs) | xs <= 0 = guardFailed

makeADTSpec ''Queue

main = runRufousWithOptions args{signature=_Queue, averageDugSize=1000, numberOfTests=5}
