{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module ADTs.List.Definition where

import Test.Rufous

class Listy t where
   lempty :: t a
   lcons :: a -> t a -> t a
   ltail :: t a -> t a
   lhead :: t a -> a
   lappend :: t a -> t a -> t a
   lnull :: t a -> Bool

instance Listy [] where
   lempty = []
   lcons = (:)
   ltail = tail
   lhead = head
   lappend = (++)
   lnull = null

newtype Shadow a = S Int
instance Listy Shadow where
   lempty = S 0
   lcons _ (S n) = S (n+1)
   ltail (S 0) = guardFailed
   ltail (S n) = S (n - 1)
   lhead (S 0) = guardFailed
   lhead (S _) = shadowUndefined
   lappend (S n) (S m) = S (n + m)
   lnull _ = shadowUndefined

makeADTSignature ''Listy