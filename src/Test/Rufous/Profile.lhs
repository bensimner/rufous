> {-# LANGUAGE TemplateHaskell #-}

Usage of a data structure can be concisely described by a set of usage statistics.
The `Profile' type defined in this file is one such set of statistics

> module Test.Rufous.Profile where
> 
> import Lens.Micro
> import Lens.Micro.TH
> import qualified Data.Map as M
> import Data.Aeson
> import Data.String (fromString)

The Profile contains multiple important characteristics:
    - The weights of proportion for each of the applications of the operations
    - The weights of persistent applications of those operations
    - The weights of dead/mutated notes (the mortality)
        which controls the relative "depth" of the DUG

> data Profile =
>   Profile
>       { _operationWeights :: M.Map String Float
>       , _persistentApplicationWeights :: M.Map String Float
>       , _mortality :: Float
>       }
>   deriving (Show)
> makeLenses ''Profile

> instance ToJSON Profile where
>   toJSON p = object [ (fromString "weights", (toJSON (p ^. operationWeights)))
>                     , (fromString "persistents", (toJSON (p ^. persistentApplicationWeights))) 
>                     , (fromString "mortality", toJSON (p ^. mortality))
>                     ]

Extracting information
----------------------

> weights :: Profile -> [(String, Float)]
> weights p = M.toList $ p ^. operationWeights
>
> persistentProb :: Profile -> String -> Float
> persistentProb p s = (p ^. persistentApplicationWeights) M.! s
