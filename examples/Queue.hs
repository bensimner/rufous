{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Main where
import Prelude hiding (head, tail)
import qualified Prelude as P

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

class Queue q where
   snoc :: a -> q a -> q a
   empty :: q a 
   tail :: q a -> q a
   head :: q a -> a

-- simple list implementation
newtype ListQueue a = ListQueue [a]
   deriving (Show)

instance Queue ListQueue where
   snoc x (ListQueue xs) = ListQueue (xs ++ [x])
   empty     = ListQueue []
   head (ListQueue xs) = P.head xs
   tail (ListQueue xs) = ListQueue $ P.tail xs


-- double list batched queue
data BQueue a = BQ [a] [a]
   deriving (Show)

bq [] back = BQ (reverse back) []
bq f b = BQ f b

instance Queue BQueue where
   empty = BQ [] []
   snoc x (BQ front back) = bq front (x:back)
   tail (BQ (_:fs) back) = bq fs back
   head (BQ (f:_) _) = f


-- Triple list rotating queue

data RQueue a = RQ [a] [a] [a]
   deriving (Show)

rq fr b (s:ss) = RQ fr b ss
rq fr b [] = RQ f' [] f'
   where
      f' = rotate fr b []
      rotate [] [b] ss = b:ss
      rotate (f:fs) (b:bs) ss = f : rotate fs bs (b:ss)
      

instance Queue RQueue where
   empty = RQ [] [] []
   snoc x (RQ f b ss) = rq f (x:b) ss
   head (RQ (f:_) _ _) = f
   tail (RQ (_:f) b ss) = rq f b ss

data ShadowQueue x = ShadowQueue Int
   deriving (Show)

instance Queue ShadowQueue where
   snoc x (ShadowQueue xs) = ShadowQueue (xs + 1)
   empty     = ShadowQueue 0
   head (ShadowQueue xs) | xs > 0  = shadowUndefined
   head (ShadowQueue xs) | xs <= 0 = guardFailed

   tail (ShadowQueue xs) | xs > 0  = ShadowQueue (xs - 1)
   tail (ShadowQueue xs) | xs <= 0 = guardFailed

makeADTSpec ''Queue

main = runRufousWithOptions args{signature=_Queue, averageDugSize=10000, numberOfTests=1}
