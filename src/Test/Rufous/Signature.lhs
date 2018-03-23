An Abstract-Data-Type describes an API for interacting with a datatype
The `Signature' type describes that API

> {-# LANGUAGE StandaloneDeriving, TemplateHaskell, ExistentialQuantification #-}
> module Test.Rufous.Signature where
>
> import Lens.Micro
> import Lens.Micro.TH
>
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
>   deriving (Eq, Show, Ord)
>
> data ImplType = forall t. (Show t, Typeable t) => ImplType t
> data Implementation = 
>   Implementation
>       { _implName :: String
>       , _implOperations :: M.Map String (Dynamic, ImplType)
>       }
> makeLenses ''Implementation
>
> instance Show Implementation where
>   show (Implementation impl _) = "<" ++ impl ++ ">"
> instance Eq Implementation where
>   i1 == i2 = i1 ^. implName == i2 ^. implName
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

Signature specific operations over profiles
===========================================


> pmf :: Signature -> P.Profile -> Float
> pmf s p = sum pmfs / (fromIntegral (length pmfs))
>   where mutators = [k | (k, o) <- M.toList (s ^. operations), Mutator == (o ^. opSig ^. opType)]
>         pmfs = [(p ^. P.persistentApplicationWeights) M.! k | k <- mutators]

> pof :: Signature -> P.Profile -> Float
> pof s p = sum pofs / (fromIntegral (length pofs))
>   where observers = [k | (k, o) <- M.toList (s ^. operations), Observer == (o ^. opSig ^. opType)]
>         pofs = [(p ^. P.persistentApplicationWeights) M.! k | k <- observers]
