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
>    , OpArgs(..)
>    , OpArg(..)
>    , OperationType(..)
>    , OperationSig(..)
>    ) where
> 
> import Data.List (intercalate)
> import qualified Data.Map as M
> import Test.Rufous.SigParser

An ADT is simply a collection of operations, tagged with a unqiue name.

> data Signature st =
>    Signature
>       { operations :: M.Map OperationName (Operation st)
>       , implementations :: [Implementation]
>       , initialState :: st
>       }

An operation of an ADT has a name, and a signature (see: SigParser.lhs)

> data Operation st =
>    Simple
>       { opName     :: OperationName
>       , sig        :: OperationSig
>       , pre        :: OpArgs st -> Bool
>       , transition :: OpArgs st -> st
>       , post       :: OpArgs st -> Int -> Bool
>       }
> instance Eq (Operation st) where
>   o1 == o2 = (opName o1 == opName o2) && (sig o1 == sig o2)
> instance Show (Operation st) where
>   show (Simple name s _ _ _) = "{" ++ show name ++ ": " ++ show s ++ "}"

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

To manage pre-conditions we define a non-deterministic (in)finite state machine,
now each operation can be guarded by a pre-condition over states:

> type OpArgs st = [OpArg st]
> data OpArg st = VersionArg st | IntArg Int
>   deriving (Eq, Show)

We expose some builder functions that make it easy to construct these maps from basic tupled lists

> signature :: String -> [Operation st] -> [Implementation] -> Signature st
> signature name ops impls = Signature (M.fromList [(opName o, o) | o <- ops]) impls (error "signature :: unspecified initial state")
> 
> implementation :: TypeName -> [(OperationName, String)] -> Implementation
> implementation tyName ops = (tyName, M.fromList ops)
> 
> operation :: String -> String -> Operation st
> operation name typeSig = Simple name (parseSig typeSig) (const True) (\_ -> error $ "no transition function for " ++ name) (\_ _ -> True)
>
> filterType :: OperationType -> Signature st -> [Operation st]
> filterType t s = filter ((== t) . opType . sig) (M.elems $ operations s)
> 
> generators :: Signature st -> [Operation st]
> generators = filterType Generator
> 
> mutators :: Signature st -> [Operation st]
> mutators = filterType Mutator
> 
> observers :: Signature st -> [Operation st]
> observers = filterType Observer
> 
> isType :: Signature st -> OperationType -> Operation st -> Bool
> isType s t op = op `elem` (filterType t s)
