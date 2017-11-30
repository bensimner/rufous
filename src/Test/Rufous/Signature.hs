module Test.Rufous.Signature where

type OperationId = String

-- Here we make simplifying assumption (for now) that non-version arguments are of type Int
data Arg =
      Version
    | Arg (Maybe Int)

    deriving (Show)

data OperationType = Generator | Observer | Mutator
    deriving (Show, Eq)

-- TODO: Remove this.
instance Show (a -> b) where
    show f = "<function>"

data OperationSignature =
    OperationSignature {
          opArgs :: [Arg]
        , opType :: OperationType
    }

    deriving (Show)

data Operation state =
    Simple {
          opName :: String
        , sig :: OperationSignature
        , pre :: state -> Bool
        , post :: state -> [Arg] -> Int -> state
    }
    deriving (Show)

parseSig :: String -> OperationSignature
parseSig sig = OperationSignature [] Generator

operation :: String -> String -> Operation a
operation name signature = Simple name (parseSig signature) (const True) (\x _ _ -> x)

-- ADT Signature
data Signature state =
   Signature {
        tyName :: String            -- Name of the ADT type T
        , operations :: [Operation state] -- List of operations over T
        , initialState :: state
   }

   deriving (Show)

signature :: Signature state
signature = Signature undefined undefined undefined

allOperationsOfType :: OperationType -> Signature state -> [Operation state]
allOperationsOfType t s = filter ((== t) . opType . sig) (operations s)

mutators :: Signature state -> [Operation state]
observers :: Signature state -> [Operation state]
generators :: Signature state -> [Operation state]
mutators = allOperationsOfType Mutator
observers = allOperationsOfType Observer
generators = allOperationsOfType Generator