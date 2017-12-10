> module Test.Rufous.Signature
>    ( Operation(..)
>    , Signature(..)
>    , signature
>    , operation
>    , implementation
>    , Implementation
>    , generators
>    , observers
>    , mutators
>    , isType
> 
>    -- Extract information from module path
>    , moduleFromType
>    , nameFromType
>
>    -- utilities
>    , sig2str
>    , Arg(..)
>    , OperationType(..)
>    , OperationSig(..)
>    ) where
> 
> import Data.List (intercalate)
> import qualified Data.Map as M
> import Test.Rufous.SigParser

An ADT is simply a collection of operations, tagged with a unqiue name.

> data Signature =
>    Signature
>       { operations :: M.Map OperationName Operation
>       , implementations :: [Implementation]
>       }
>    deriving (Show)

An operation of an ADT has a name, and a signature (see: SigParser.lhs)

> data Operation =
>    Simple
>       { opName :: OperationName
>       , sig    :: OperationSig
>       }
>    deriving (Eq, Show)

Implementations of the ADT are simply a fully-qualified type name and text implementations for each operation

> type Implementation = (TypeName, M.Map OperationName OperationCode)
> type TypeName = String
> type OperationName = String
> type OperationCode = String
>
> splitType :: TypeName -> [String]
> splitType xs = go xs ""
>   where
>       go [] s = [s]
>       go ('.' : xs) s = s : go xs ""
>       go (x : xs) s = go xs (s ++ [x])
>
> moduleFromType :: TypeName -> String
> moduleFromType ty = intercalate "." $ init $ splitType ty
>
> nameFromType :: TypeName -> String
> nameFromType ty = intercalate "" $ splitType ty

We expose some builder functions that make it easy to construct these maps from basic tupled lists

> signature :: String -> [Operation] -> [Implementation] -> Signature
> signature name ops impls = Signature (M.fromList [(opName o, o) | o <- ops]) impls
> 
> implementation :: TypeName -> [(OperationName, String)] -> Implementation
> implementation tyName ops = (tyName, M.fromList ops)
> 
> operation :: String -> String -> Operation
> operation name typeSig = Simple name (parseSig typeSig)
>
> filterType :: OperationType -> Signature -> [Operation]
> filterType t s = filter ((== t) . opType . sig) (M.elems $ operations s)
> 
> generators :: Signature -> [Operation]
> generators = filterType Generator
> 
> mutators :: Signature -> [Operation]
> mutators = filterType Mutator
> 
> observers :: Signature -> [Operation]
> observers = filterType Observer
> 
> isType :: Signature -> OperationType -> String -> Bool
> isType s t name = name `elem` map opName (filterType t s)
