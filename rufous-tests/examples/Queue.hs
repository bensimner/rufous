{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where
import Prelude hiding (head, tail)
import qualified Prelude as P

import Test.Rufous
   ( RufousOptions(..)
   , makeADTSignature
   , shadowUndefined
   , guardFailed
   , extractorUndefined
   , mainWith
   , args )

class Queue q where
   snoc :: a -> q a -> q a
   empty :: q a
   tail :: q a -> q a
   head :: q a -> a

   extractShadow :: q a -> ShadowQueue a
   extractShadow = extractorUndefined

-- simple list implementation
newtype ListQueue a = ListQueue [a]
   deriving (Show)

instance Queue ListQueue where
   snoc x (ListQueue xs) = ListQueue (xs ++ [x])
   empty = ListQueue []
   head (ListQueue xs) = P.head xs
   tail (ListQueue xs) = ListQueue $ P.tail xs

   extractShadow (ListQueue xs) = ShadowQueue xs


-- double list batched queue
data BQueue a = BQ [a] [a]
   deriving (Show)

bq :: [a] -> [a] -> BQueue a
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

rq :: [a] -> [a] -> [a] -> RQueue a
rq fr b (_:ss) = RQ fr b ss
rq fr b0 [] = RQ f' [] f'
   where
      f' = rotate fr b0 []
      rotate [] [b] ss = b:ss
      rotate (f:fs) (b:bs) ss = f : rotate fs bs (b:ss)


instance Queue RQueue where
   empty = RQ [] [] []
   snoc x (RQ f b ss) = rq f (x:b) ss
   head (RQ (f:_) _ _) = f
   tail (RQ (_:f) b ss) = rq f b ss

data ShadowQueue x = ShadowQueue [x]
   deriving (Show,Eq)

instance Queue ShadowQueue where
   snoc x (ShadowQueue xs) = ShadowQueue (xs ++ [x])
   empty     = ShadowQueue []
   head (ShadowQueue (_:y:_)) = y
   head (ShadowQueue [y]) = y
   head (ShadowQueue []) = guardFailed

   tail (ShadowQueue (_:xs))  = ShadowQueue xs
   tail (ShadowQueue []) = guardFailed

makeADTSignature ''Queue

main :: IO ()
main = mainWith args{signature=_Queue, verbose=True, averageDugSizes=[10, 100], numberOfTests=5}
