{-# LANGUAGE TemplateHaskell #-}
module Test.Rufous.Internal.Aggregation.Types
   ( AggregatorType(..)
   , AggregationOptions(..)

   -- | KMeans aggregator
   , KMeansOptions(..)
   , kmeansArgs

   -- | Aggregated Result
   , AggregatedResult(..)
   , aggResult
   , aggResults

   -- | default options
   , aggregationArgs
   )
where

import Control.Lens

import Test.Rufous.Internal.Evaluation.Types

data KMeansOptions =
   KMeansOptions
      { numMeans :: Int
      }
   deriving (Show)

data AggregatorType =
     KMeans
   deriving (Show)

data AggregationOptions =
   AggregationOptions
      { kmeansOptions :: KMeansOptions
      }
   deriving (Show)

-- | An 'AggregatedResult' is many results that have been placed into one 'Result' type
data AggregatedResult =
   AggregatedResult
      { _aggResults :: [Result]
      , _aggResult  :: Result
      }
   deriving (Show)
makeLenses ''AggregatedResult  -- the other types are not lensy so that the API is clean

kmeansArgs :: KMeansOptions
kmeansArgs = KMeansOptions 5

aggregationArgs :: AggregationOptions
aggregationArgs = AggregationOptions kmeansArgs
