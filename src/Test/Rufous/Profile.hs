module Test.Rufous.Profile where

import qualified Test.Rufous.Signature as Sig

data UserProfile =
    UserProfile {
        operationWeights :: [(String, Int)]
        , propPersistentMutation :: Rational
        , propPersistentObservation :: Rational
    }

    deriving (Show)

data Profile =
    Profile {
        mutatorProb :: [(String, Rational)]
        , generatorProb :: [(String, Rational)]
        , observerProb :: [(String, Rational)]
        , propPersistentMutations :: Rational
        , propPersistentObservations :: Rational
    }

pMutator :: Sig.Signature -> Profile -> Rational
pMutator s p = sum $ map snd $ mutatorProb p

pGenerator :: Sig.Signature -> Profile -> Rational
pGenerator s p = sum $ map snd $ generatorProb p

pObserver :: Sig.Signature -> Profile -> Rational
pObserver s p = sum $ map snd $ observerProb p