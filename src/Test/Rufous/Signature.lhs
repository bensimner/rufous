> module Test.Rufous.Signature
>    ( Operation(..)
>    , Signature(..)
>    , signature
>    , operation
>    , Arg(..)
>    , OperationType(..)
>    , OperationSig(..)
>    ) where
> 
> import qualified Data.Map as M
> import Test.Rufous.SigParser

An ADT is simply a collection of operations, tagged with a unqiue name.

> data Signature =
>    Signature
>       { operations :: M.Map String Operation
>       }
>    deriving (Show)

An operation of an ADT has a name, and a signature (see: SigParser.lhs)

> data Operation =
>    Simple
>       { opName :: String
>       , sig    :: OperationSig
>       }
>    deriving (Eq, Show)

We expose some builder functions that make it easy to construct these maps from basic tupled lists

> signature :: String -> [Operation] -> Signature
> signature name ops = Signature (M.fromList [(opName o, o) | o <- ops])
> 
> operation :: String -> String -> Operation
> operation name typeSig = Simple name (parseSig typeSig)
