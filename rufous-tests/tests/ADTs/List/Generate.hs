{-# LANGUAGE BangPatterns #-}
module ADTs.List.Generate where

import ADTs.List.Definition

import System.Random

type St t = ([t Int], [Int])

chooseFrom :: [a] -> IO a
chooseFrom xs = do
   i <- randomRIO (0, length xs - 1)
   return $ xs !! i

genCons :: Listy t => St t -> IO (St t)
genCons (xss, obs) = do
   x <- randomIO
   xs <- chooseFrom xss
   print ("cons")
   return $ ((lcons x xs):xss, obs)

genEmpty :: Listy t => St t -> IO (St t)
genEmpty (xss, obs) = return (lempty:xss, obs)

genHead :: Listy t => St t -> IO (St t)
genHead (xss, obs) = do
      xs <- chooseFrom xss
      print ("head")
      return $ if lnull xs then
                  (xss, obs)
               else
                  (xss, (lhead xs):obs)

genTail :: Listy t => St t -> IO (St t)
genTail (xss, obs) = do
      xs <- chooseFrom xss
      print ("tail")
      return $ ((doTail xs) : xss, obs)
   where
      doTail xs | lnull xs = xs
      doTail xs = ltail xs

genAppend :: Listy t => St t -> IO (St t)
genAppend (xss, obs) = do
      xs  <- chooseFrom xss
      xs' <- chooseFrom xss
      print ("append")
      return $ (lappend xs xs' : xss, obs)

generateCall :: Listy t => St t -> IO (St t)
generateCall xss = do
   a <- chooseFrom [genEmpty xss, genCons xss, genHead xss, genTail xss, genAppend xss]
   a

generateCalls :: Listy t => t Int -> Int -> IO (St t)
generateCalls e n = do
      (xss, obs) <- go ([e],[]) n
      return (xss, obs)
   where go s 0 = return s
         go s n = do
            s' <- generateCall s
            go s' (n-1)
