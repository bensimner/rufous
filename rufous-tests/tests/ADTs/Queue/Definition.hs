{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module ADTs.Queue.Definition where

import Test.Rufous

class Queuey t where
   qempty :: t a
   qsnoc :: t a -> a -> t a
   qtail :: t a -> t a
   qhead :: t a -> a
   qnull :: t a -> Bool

instance Queuey [] where
   qempty = []
   qsnoc t x = t ++ [x]
   qtail (x:xs) = xs
   qhead (x:xs) = x
   qnull xs = null xs

newtype Shadow a = S Int
instance Queuey Shadow where
   qempty = S 0
   qsnoc (S n) _ = S (n+1)
   qtail (S 0) = guardFailed
   qtail (S n) = S (n - 1)
   qhead (S 0) = guardFailed
   qhead (S _) = shadowUndefined
   qnull _ = shadowUndefined

makeADTSignature ''Queuey
