module Test.Rufous.Exceptions where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

-- | Exceptions that are exposed to the user
data RufousException =
      MissingNullImplementation String  -- with name of ADT
    | ShadowTypeMismatch String String  -- with types
   deriving (Show, Typeable)
instance Exception RufousException
