{-# LANGUAGE ExistentialQuantification #-}
module Test.Rufous.Internal.Table where

import Data.List (intercalate)

data Table s =
   Table
      { header :: [s]
      , rows :: [[s]]
      }
   deriving (Show)


widths :: Table String -> [Int]
widths t = go (rows t) (width (header t))
   where
      width = map length
      go (r:rs) m = go rs (map (uncurry max) (zip (width r) m))
      go [] m = m

padding :: Table String -> Table Int
padding t = Table hd rs
   where
      padCol (c, w) = ((w + 2) - length c)
      ws = widths t
      pad r = map padCol (zip r ws)
      hd = pad (header t)
      rs = map pad (rows t)

zipTables :: Table a -> Table b -> Table (a, b)
zipTables t1 t2 = Table hd rs
   where hd = zip (header t1) (header t2)
         rs = map (uncurry zip) $ zip (rows t1) (rows t2)

render :: Table String -> String
render t = draw $ [drawBar "~" hd, drawRow hd, drawBar "+" hd] ++ (map drawRow (rows padded))
   where annotated = zipTables t (padding t)
         hd = header padded
         padded = doPadding annotated
         drawRow r = intercalate "|" r
         drawBar c r = intercalate c [replicate (length c) '~' | c <- r]
         draw rs = intercalate "\n" rs

doPadding :: Table (String, Int) -> Table String
doPadding t = Table hd rs
   where padRow r = 
            [let (p1, p2) = splitPad p in
             replicate p1 ' ' ++ c ++ replicate p2 ' ' | (c, p) <- r]
         hd = padRow (header t)
         rs = map padRow (rows t)

-- Split a padding of N total whitespace chars into (K, L) chars
-- such that K + L == N
splitPad :: Int -> (Int, Int)
splitPad p =
   if (p `mod` 2) == 0 then
      (p `div` 2, p `div` 2)
   else
      (1 + (p `div` 2), p `div` 2)
