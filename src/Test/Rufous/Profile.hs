module Test.Rufous.Profile where

import qualified Data.Map as M

type ProfileEntry = M.Map String Float

data Profile =
   Profile
      { mutatorWeights :: ProfileEntry
      , observerWeights :: ProfileEntry
      , generatorWeights :: ProfileEntry
      , persistentMutationWeight :: Float       -- must be between 0 and 1
      , persistentObservationWeight :: Float    -- must be between 0 and 1
      }

allWeights :: Profile -> ProfileEntry
allWeights p = M.fromList $ concat (M.toList <$> [mutatorWeights p, observerWeights p, generatorWeights p])

normaliseProfile :: Profile -> Profile
normaliseProfile p =
   Profile (f $ mutatorWeights p)
           (f $ observerWeights p)
           (f $ generatorWeights p)
           (persistentMutationWeight p)
           (persistentObservationWeight p)
   where
      pr = sum $ allWeights p
      f m = (/ pr) <$> m

normaliseWeights :: ProfileEntry -> ProfileEntry
normaliseWeights ws = (/ pr) <$> ws
   where
      pr = sum ws

pMutator :: Profile -> Float
pMutator p = sum $ mutatorWeights p
pObserver p = sum $ observerWeights p
pGenerator p = sum $ generatorWeights p

pOperation :: Profile -> String -> Float
pOperation p o = (allWeights p) M.! o
