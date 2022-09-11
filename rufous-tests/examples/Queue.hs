{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where
import Prelude hiding (head, tail)
import qualified Prelude as P

import Test.Rufous
   ( RufousOptions(..)
   , OutputOptions(..)
   , makeADTSignature
   , shadowUndefined
   , guardFailed
   , mainWith
   , args
   , outputArgs
   )

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
   empty = ListQueue []
   head (ListQueue xs) = P.head xs
   tail (ListQueue xs) = ListQueue $ P.tail xs


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
   tail (RQ (_:fs) b ss) = rq fs b ss


-- the Shadow implementation


data ShadowQueue x = ShadowQueue Int
   deriving (Show,Eq)

instance Queue ShadowQueue where
   snoc x (ShadowQueue n) = ShadowQueue (n + 1)
   empty     = ShadowQueue 0
   head (ShadowQueue 0) = guardFailed
   head (ShadowQueue _) = shadowUndefined
   tail (ShadowQueue 0) = guardFailed
   tail (ShadowQueue n) = ShadowQueue (n - 1)

makeADTSignature ''Queue

main :: IO ()
main = mainWith
         args
            { signature=_Queue
            , averageDugSizes=[20]
            , numberOfTests=1 -- number of DUGs to generate
            , verbose=False
            , outputOptions=outputArgs { dumpDUGs=1, dumpDUGDetail=2 }
            }

{- EXAMPLE OUTPUT
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 #dugs |  size | "empty" weight | "head" weight | "snoc" weight | "tail" weight | mortality |  pmf |  pof | Main.RQueue | Main.BQueue | Main.ListQueue 
~~~~~~~+~~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~+~~~~~~~~~~~+~~~~~~+~~~~~~+~~~~~~~~~~~~~+~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~
     2 |     0 |           0.00 |          0.00 |          0.00 |          0.00 |      0.00 | 0.00 | 0.00 |    9.57e-9s |    7.21e-8s |       1.79e-8s 
    71 |  1440 |           0.32 |          0.12 |          0.29 |          0.28 |      0.27 | 0.66 | 1.00 |    7.38e-4s |    8.61e-4s |       7.16e-4s 
     3 |    41 |           0.93 |          0.00 |          0.07 |          0.00 |      0.10 | 0.38 | 0.00 |    3.29e-6s |    4.61e-6s |       3.56e-6s 
    24 |    77 |           0.27 |          0.16 |          0.25 |          0.32 |      0.30 | 0.57 | 1.00 |    1.01e-5s |    1.11e-5s |       1.04e-5s 
   100 | 27593 |           0.26 |          0.24 |          0.26 |          0.23 |      0.40 | 0.42 | 1.00 |       0.06s |       0.08s |          0.10s 
-}