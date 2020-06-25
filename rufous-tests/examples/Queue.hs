{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where
import Prelude hiding (head, tail)
import qualified Prelude as P

-- import qualified pqueue
-- import qualified thrist

import Test.Rufous
   ( RufousOptions(..)
   , OutputOptions(..)
   , makeADTSignature
   , shadowUndefined
   , guardFailed
   , extractorUndefined
   , mainWith
   , args
   , outputArgs
   )

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
   head (ShadowQueue (y:_)) = y
   head (ShadowQueue []) = guardFailed

   tail (ShadowQueue (_:xs))  = ShadowQueue xs
   tail (ShadowQueue []) = guardFailed

makeADTSignature ''Queue

main :: IO ()
main = mainWith
         args
            { signature=_Queue
            , averageDugSizes=[10000]
            , numberOfTests=50 -- number of DUGs to generate

            , info=True -- equivalent to saying verbosity=1
            , outputOptions=
               outputArgs
                  { dumpDUGs=False  -- set to True to create output/dugName.pdf graphviz output
                  , dumpDUGDetail=2 -- show shadow in the graphviz output
                  }
            }

{- EXAMPLE OUTPUT
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 #dugs | #size | "empty" weight | "head" weight | "snoc" weight | "tail" weight | mortality |  pmf |  pof | Main.ListQueue | Main.BQueue | Main.RQueue
~~~~~~~+~~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~+~~~~~~~~~~~+~~~~~~+~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~+~~~~~~~~~~~~~
     7 |  6016 |           0.10 |          0.27 |          0.52 |          0.10 |      0.34 | 0.59 | 1.00 |          0.01s |       0.01s |    9.11e-3s
    13 |    82 |           0.45 |          0.21 |          0.14 |          0.21 |      0.18 | 0.62 | 0.97 |       3.06e-5s |    3.27e-5s |    2.95e-5s
    13 |   205 |           0.41 |          0.23 |          0.13 |          0.23 |      0.15 | 0.70 | 1.00 |       9.31e-5s |    7.51e-5s |    9.41e-5s
     7 |  9063 |           0.39 |          0.20 |          0.18 |          0.23 |      0.21 | 0.64 | 1.00 |          0.01s |       0.05s |       0.10s
    10 |  1673 |           0.27 |          0.23 |          0.21 |          0.29 |      0.22 | 0.66 | 1.00 |       1.89e-3s |    1.79e-3s |    2.18e-3s

-}