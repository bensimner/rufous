{-# LANGUAGE BangPatterns #-}
module ADTs.Queue.Generate where

import ADTs.Queue.Definition

import System.Random

type St t = ([t Int], [Int])

chooseFrom :: [a] -> IO a
chooseFrom xs = do
   i <- randomRIO (0, length xs - 1)
   return $ xs !! i

genSnoc :: Queuey t => St t -> IO (St t)
genSnoc (xss, obs) = do
   xs <- chooseFrom xss
   x <- randomIO
   print ("snoc")
   return $ ((qsnoc xs x):xss, obs)

genEmpty :: Queuey t => St t -> IO (St t)
genEmpty (xss, obs) = return (qempty:xss, obs)

genHead :: Queuey t => St t -> IO (St t)
genHead (xss, obs) = do
      xs <- chooseFrom xss
      print ("head")
      return $ if qnull xs then
                  (xss, obs)
               else
                  (xss, (qhead xs):obs)

genTail :: Queuey t => St t -> IO (St t)
genTail (xss, obs) = do
      xs <- chooseFrom xss
      print ("tail")
      return $ ((doTail xs) : xss, obs)
   where
      doTail xs | qnull xs = xs
      doTail xs = qtail xs

generateCall :: Queuey t => St t -> IO (St t)
generateCall xss = do
   a <- chooseFrom [genEmpty xss, genSnoc xss, genHead xss, genTail xss]
   a

generateCalls :: Queuey t => t Int -> Int -> IO (St t)
generateCalls e n0 = do
      (xss, obs) <- go ([e],[]) n0
      return (xss, obs)
   where go s 0 = return s
         go s n = do
            s' <- generateCall s
            go s' (n-1)
