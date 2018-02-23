An Abstract-Data-Type describes an API for interacting with a datatype
The `Signature' type describes that API

> {-# LANGUAGE StandaloneDeriving, TemplateHaskell, ExistentialQuantification #-}
> module Test.Rufous.Signature where
>
> import Control.Lens
> import Data.Dynamic (Dynamic, Typeable)
>
> import qualified Data.Map as M
> import qualified Data.Set as S
> 
> import qualified Test.Rufous.Profile as P
>
> data Null x = NullImpl
>   deriving (Show, Eq, Typeable)
>
> type ArgType = Arg () () () ()
> data NVA n i b = VersionParam n | IntArg i | BoolArg b
>   deriving (Eq, Show)
>
> data Arg v n i b = Version v | NonVersion (NVA n i b)
>   deriving (Eq, Show)
>
> data OperationType = Mutator | Observer | Generator
>   deriving (Eq, Show)
>
> data ImplType = forall t. (Show t, Typeable t) => ImplType t
> data Implementation = 
>   Implementation
>       { _implName :: String
>       , _implOperations :: M.Map String (Dynamic, ImplType)
>       }
> instance Show Implementation where
>   show (Implementation impl _) = "<" ++ impl ++ ">"
> makeLenses ''Implementation
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
> type ShadowImplementation = Implementation
> data Signature =
>   Signature
>       { _operations :: M.Map String Operation
>       , _implementations :: [Implementation]
>       , _nullImpl :: Implementation
>       , _nullExtractorImpl :: Implementation
>       , _shadowImpl :: Maybe ShadowImplementation
>       }
>   deriving (Show)
> makeLenses ''Signature

Obtaining information from the Signature is very easy with some simple combinators:

> operationNames :: Signature -> [String]
> operationNames s = M.keys $ s ^. operations

Convenience functions and instances:
