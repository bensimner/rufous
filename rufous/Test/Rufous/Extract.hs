module Test.Rufous.Extract where

import qualified Test.Rufous.DUG as D
import qualified Test.Rufous.Signature as S


type ExtractArg t x = S.Arg (WrappedADT t x) x Int Bool
type ExtractedDUG = D.DUG
data WrappedADT t x =
  WrappedADT
      { nodeId :: Int
      , nodeArgs :: [ExtractArg t x]
      , nodeOp :: String
      , value :: Either x (t x)  -- this is either an observeration node or an operation node
      }
  deriving (Show)

extract :: IO a -> IO (a, ExtractedDUG)
extract = undefined

_log_operation :: String -> [ExtractArg t x] -> t x -> WrappedADT t x
_log_operation opName args x = WrappedADT 0 args opName (Right x)

_log_observer :: String -> [ExtractArg t x] -> x -> x
_log_observer _ _ x = x

getVersion :: WrappedADT t x -> t x
getVersion w = case value w of
   Right x -> x
