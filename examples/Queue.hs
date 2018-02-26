{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Main where

import Test.Rufous (makeRufousSpec, runRufous)

class Queue q where
   snoc :: a -> q a -> q a
   empty :: q a 

newtype ListQueue a = ListQueue [a]
   deriving (Show)

instance Queue [] where
   snoc x xs = xs ++ [x]
   empty     = []

instance Queue ListQueue where
   snoc x (ListQueue xs) = ListQueue (xs ++ [x])
   empty     = ListQueue []

makeRufousSpec ''Queue

main = runRufous _Queue
