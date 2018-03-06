{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Main where

import Test.Rufous 
   ( makeRufousSpec
   , runRufous
   , runRufousWithOptions
   , defaultOptions
   , RufousOptions(..)
   , shadowUndefined
   , guardFailed )

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

data ShadowQueue x = ShadowQueue Int
   deriving (Show)

instance Queue ShadowQueue where
   snoc x (ShadowQueue xs) = ShadowQueue (xs + 1)
   empty     = ShadowQueue 0
   head' (ShadowQueue xs) | xs > 0  = shadowUndefined
   head' (ShadowQueue xs) | xs <= 0 = guardFailed

makeRufousSpec ''Queue

main = runRufous _Queue
