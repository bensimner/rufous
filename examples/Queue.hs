module Main where

class Queue q where
   snoc :: a -> q a -> q a
   empty :: q a 
