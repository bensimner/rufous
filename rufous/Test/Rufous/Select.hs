{-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}
module Test.Rufous.Select where

import Control.Lens

import qualified Data.Map as M

import qualified Test.Rufous.Profile as P
import qualified Test.Rufous.Signature as S
import qualified Test.Rufous.Run as R
import qualified Test.Rufous.Aggregate as Agg

import qualified Test.Rufous.Internal.Table as T

-- | Given a list of annotated (normalised) DUGs perform a selection step
-- which prints some information about the runtime
select :: S.Signature -> [Agg.AggregatedResult] -> IO ()
select s rs =
   let t = makeTable s rs in
   putStrLn $ T.render t

makeTable :: S.Signature -> [Agg.AggregatedResult] -> T.Table String
makeTable s rs = T.Table (makeHeader s) (map (\r -> makeRow s r) rs)

makeHeader :: S.Signature -> [String]
makeHeader s = ["Ntests"]
               ++ [opName | opName <- M.keys (s ^. S.operations)]
               ++ ["mortality"]
               ++ [i ^. S.implName | i <- s^.S.implementations]

makeRow :: S.Signature -> Agg.AggregatedResult -> [String]
makeRow s ar = [show (length (ar ^. Agg.aggResults))]
               ++ [(getWeight p opName) | opName <- M.keys (s ^. S.operations)]
               ++ [show (p^.P.mortality)]
               ++ [show ((tinfo^.R.times) M.! i) | i <- s^.S.implementations]
   where p = r ^. R.resultProfile
         tinfo = r ^. R.resultTimes
         r = ar ^. Agg.aggResult

getWeight :: P.Profile -> String -> String
getWeight p opName =
   case M.lookup opName (p ^. P.operationWeights) of
      Just it -> show it
      Nothing -> "N/A"

{-
-- A "row" in the DUG timing tables
 - dug:
 -    operation | count | impl1 total | impl1 avg | impl2 total | impl2 avg | ...
 -    ==========+=======+=============+===========+=============+===========+=====
 -    empty     | 3     | 12s         | 4s        | 6s          | 2s        | ...
data TDUG = forall a. T {tdug :: R.TimingDug a}

data OpRecord = 
   OpRecord 
      { _opName :: String
      , _opCount :: Int 
      , _implTotalTimes :: ImplementationTimes
      , _opDug :: TDUG
      }
makeLenses ''OpRecord

-- A "row" in the summary table
 - summary:
 -    dug   | empty count | snoc count | head count | tail count | mortality | pmf | pof | impl1 | impl2 | impl3
 -    ======+=============+============+============+============+===========+=====+=====+=======+=======+=======
 -    dug 1 | 5           | 6          | 3          | 2          | 0.6       | 0.2 | 0.3 | 12s   | 4s    | 12s  
data SummaryRecord = 
   SummaryRecord 
      { _dug :: TDUG
      , _ops :: M.Map String Int  -- operation counts
      , _mortality :: Float
      , _pmf :: Float -- from profile, but condensed 
      , _pof :: Float -- ^^^^^^^^^^^^^^^^^^^^^^^^^^^
      , _implTimes :: ImplementationTimes
      }
makeLenses ''SummaryRecord
-}