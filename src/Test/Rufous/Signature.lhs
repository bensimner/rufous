An Abstract-Data-Type describes an API for interacting with a datatype
The `Signature' type describes that API

> {-# LANGUAGE TemplateHaskell #-}
> module Test.Rufous.Signature where
>
> import Control.Lens
> import Data.Dynamic (Dynamic)
>
> import qualified Data.Map as M
> import qualified Data.Set as S
> 
> import qualified Test.Rufous.DUG as D
> import qualified Test.Rufous.Profile as P
>
> type ArgType = Arg () String
> data Arg v n = Version v | NonVersion n
>   deriving (Eq, Show)
>
> data OperationType = Mutator | Observer | Generator
>   deriving (Eq, Show)
>
> type Implementation = M.Map String Dynamic
> 
> data OperationSig =
>   OperationSig
>       { _opArgs :: [ArgType]
>       , _opType :: OperationType
>       , _opRet  :: ArgType
>       }
>   deriving (Show)
> makeLenses ''OperationSig
> 
> data Operation =
>   Operation 
>       { _opName :: String
>       , _opSig  :: OperationSig
>       }
>   deriving (Show)
> makeLenses ''Operation
>
> data Signature =
>   Signature
>       { _operations :: M.Map String Operation
>       , _implementations :: [Implementation]
>       }
>   deriving (Show)
> makeLenses ''Signature

Obtaining information from the Signature is very easy with some simple combinators:

> operationNames :: Signature -> [String]
> operationNames s = M.keys $ s ^. operations
