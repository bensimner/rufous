{-# LANGUAGE TemplateHaskell #-}
import Test.Rufous

import Prelude hiding (head, tail)

class Queue q where
    empty :: q a
    snoc :: a -> q a -> q a
    tail :: q a -> q a
    head :: q a -> a

-- define Shadow to guard against partial head/tail applications
newtype ShadowQueue a = ShadowQueue Int
   deriving (Show)

instance Queue ShadowQueue where
   empty = ShadowQueue 0
   snoc _ (ShadowQueue i) = ShadowQueue (i + 1)
   tail (ShadowQueue 0) = guardFailed
   tail (ShadowQueue i) = ShadowQueue (i - 1)
   head (ShadowQueue 0) = guardFailed
   head _ = shadowUndefined

-- the actual implementation we want to benchmark
newtype ListQueue a = ListQueue [a]
   deriving (Show)

instance Queue ListQueue where
    empty = ListQueue []
    snoc x (ListQueue xs) = ListQueue (xs ++ [x])
    tail (ListQueue (_:xs)) = ListQueue xs
    head (ListQueue (x:_)) = x

-- Generate Queue spec
makeADTSignature ''Queue

main :: IO ()
main = mainWith args{signature=_Queue}