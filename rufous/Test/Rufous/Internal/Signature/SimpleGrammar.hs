{-# LANGUAGE RankNTypes, GADTs #-}
-- | Operations of an ADT are described by a 'Simple' grammar
-- whose terminals are only:
--   V        for versions of the data structure
--   NV_c(t)  for non-version values with a concrete type t
--   NV_p     for non-version values with a type that is polymorphic over the version type
-- and whose only connective is ->
-- e.g.
--      `V` is simple
--      `V -> V` is simple
--      `NV_p -> NV_c(Int) -> V` is simple
--      ... etc
module Test.Rufous.Internal.Signature.SimpleGrammar where

import Data.Typeable (Typeable)
import Language.Haskell.TH (Type, pprint)
import Test.QuickCheck.Arbitrary

-- | Each argument in a DUG is either a version argument, pointing to another node in the DUG,
-- or a NonVersion argument.  Each NonVersion argument can be one of a fixed set of
-- monomorphic types or the polymorphic argument to the datatype (the so-called Version
-- Parameter)
-- This defines the so-called 'Simple' grammar:
--   e.g.   cons :: a -> [a] -> [a]  becomes
--              NonVersion (VersionParam _) -> Version _ -> Version _
--              aka NV_p -> V -> V
--          insert :: k -> v -> Map k v -> Map k v becomes
--              NonVersion (ArbArg _) -> NonVersion (VersionParam _) -> Version _
--              aka NV_c(Int) -> NV_p -> V

data Arg v n a = Version v | NonVersion (NVA n a)
  deriving (Show)
data NVA n a = VersionParam n | forall b. (Typeable b, Arbitrary b) => ArbArg a b (Maybe Type)

instance (Show n, Show a) => Show (NVA n a) where
   show (VersionParam n) = "(VersionParam " ++ show n ++ ")"
   show (ArbArg a _ (Just ty)) = "(ArbArg " ++ show a ++ " (? :: " ++ pprint ty ++ ") _ _)"
   show (ArbArg a _ Nothing) = "(ArbArg " ++ show a ++ ")"

-- ArgTypes are proxies of Arg
type ArgType = Arg () () ()

-- | Each operation is categorized into one of a Mutator, Observer or Generator.
-- Generators are operations that produce versions from arguments:
--     empty :: List a
--     singleton :: a -> List a
--     new :: Int -> Queue a
--
-- Mutators are operations that take a version and modify it, returning the new one:
--     cons :: a -> List a -> List a
--     snoc :: Queue a -> a -> Queue a
--     diff :: Set a -> Set a -> Set a
--
-- Observers are those that take version arguments and force a value out of them:
--     head :: List a -> a
--     size :: Queue a -> Int
--     null :: List a -> Bool
data OperationCategory = Mutator | Observer | Generator
   deriving (Eq, Show, Ord)

-- Classifying a list of args into a type is straightforward:
isVersion :: ArgType -> Bool
isVersion (Version _) = True
isVersion _ = False

classifyArgs :: [ArgType] -> OperationCategory
classifyArgs args =
   if not . isVersion $ last args then
      Observer
   else
      if any isVersion (init args) then
         Mutator
      else
         Generator