> module Test.Rufous.Exceptions where

Control flow between implementations and rufous is controlled via exceptions

> import Control.Exception
> import Data.Typeable

> data RufousException =
>   GuardFailed | NotImplemented
>   deriving (Show, Typeable)
> instance Exception RufousException
