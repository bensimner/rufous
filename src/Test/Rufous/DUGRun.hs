{- EXAMPLE Output file during Eval
 - The ADT is represented as a class and instances are modules
 -}


{-# LANGUAGE OverloadedStrings #-}
module Test0 where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Time.Clock

import qualified Data.Set

class T t where
   empty :: t Int
   enqueue :: t Int -> Int -> t Int
   dequeue :: t Int -> Int

instance T Data.Set.Set where
   empty = Data.Set.empty
   enqueue t x = Data.Set.insert x t
   dequeue t = Data.Set.elemAt 0 t

-- Haskell-ish DUG
-- each v-node has type either T a or just a
v0 :: Data.Set.Set Int  -- have to seed generators to prevent monomorphism restriction hitting the observers.
v0 = empty
v1 = enqueue v0 1
v2 = enqueue v1 2
v3 = enqueue v0 3
v4 = dequeue v1
v5 = dequeue v2

-- Recording
record :: IO a -> IO (a, NominalDiffTime)
record x = do
   t1 <- getCurrentTime
   y <- x
   t2 <- getCurrentTime
   let d = diffUTCTime t2 t1
   return (y, d)

collectObservers :: IO ([Int], NominalDiffTime)
collectObservers = record $ sequence $ map return [v4, v5]

run :: a -> IO ()
run x = x `seq` return ()

collectOthers :: IO NominalDiffTime
collectOthers = do
   (_, t) <- record $ sequence $ map run [v0, v1]
   return t

collectAll :: IO TimingResult
collectAll = do
   (res, t1) <- collectObservers
   t2 <- collectOthers
   return $ TimingResult (res, t1 + t2)

newtype TimingResult = TimingResult ([Int], NominalDiffTime) deriving (Show)
instance ToJSON TimingResult where
   toJSON (TimingResult (r, t)) = object ["time" .= toJSON t, "result" .= toJSON r]

main = do
  t <- collectAll
  B.writeFile "dug_{0}.output.json" (encode t)
