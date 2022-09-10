module Test.Rufous.Internal.Utils where

import Control.Exception (Exception, throwIO)

import Numeric

-- | Format a float
--
-- Use the same format everywhere in Rufous:
--  if 0 then 0.00
--  if can be formatted with 2 digits then 1.23
--  if not then 1.23e-5  or 1.23e+5
floatFmt :: Float -> String
floatFmt f | abs f == 0 = showFFloat (Just 2) f ""
floatFmt f | abs f < 0.01 = showEFloat (Just 2) f ""
floatFmt f | abs f < 10 = showFFloat (Just 2) f ""
floatFmt f = showEFloat (Just 2) f ""

-- | Unwrap a Just
--
-- If None, throw an error
unwrapJustIO :: Exception e => e -> Maybe a -> IO a
unwrapJustIO e Nothing = throwIO e
unwrapJustIO _ (Just v) = return v


-- | Sequence monadic actions
--   evaluating each action's return to WHNF before evaluating the next
psequence :: [IO a] -> IO [a]
psequence [] = error "psequence :: expected non-empty list of actions"
psequence [a] = do
    v <- a
    v `seq` return (return v)
psequence (a:as) = do
    v <- a
    rm <- v `seq` psequence as
    return (v:rm)