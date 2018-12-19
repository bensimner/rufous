-- {-# LANGUAGE FlexibleContexts #-}
module Test.Rufous.Internal.Generate.Buffer where

import Data.Sequence ((<|))
import qualified Data.Sequence as Sq
import Control.Lens (use, (^.), (%=), (.=))

import qualified Test.Rufous.Signature as S

import Test.Rufous.Internal.Generate.Types

-- | Add a BufferedOperation to the buffer
pushBuffer :: BufferedOperation -> GenState ()
pushBuffer bop = do
   buffer %= (bop <|)

-- | Pop the next element from the buffer
popBuffer :: GenState (Maybe BufferedOperation)
popBuffer = do
   b <- peekBuffer
   case b of
      Nothing -> return Nothing
      Just x -> do
         buffer %= Sq.drop 1
         return $ Just x

-- | Pop the next element from the buffer
popBufferAll :: GenState [BufferedOperation]
popBufferAll = do
   buf <- use buffer
   buffer .= Sq.empty
   return $ sqList buf

sqList :: Sq.Seq a -> [a]
sqList = foldr (:) []

peekBuffer :: GenState (Maybe BufferedOperation)
peekBuffer = do
   buf <- use buffer
   if Sq.null buf then
      return Nothing
   else do
      let b = Sq.index buf 0
      return $ Just b

-- | Check whether a given BufferedOperation has all of its inputs satisifed.
-- That is, have all Abstract holes been filled.
satisifed :: BufferedOperation -> Bool
satisifed bop = null [() | Abstract _ _ <- bop^.bufArgs]

-- | Check whether a given BufferedOperation has all of its version arguments satisifed.
-- That is, have all Abstract holes with versions been filled.
partiallySatisifed :: BufferedOperation -> Bool
partiallySatisifed bop = null [() | Abstract (S.Version _) _ <- bop^.bufArgs]
