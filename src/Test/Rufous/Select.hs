{-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}
module Test.Rufous.Select where

import Control.Lens

import qualified Data.Map as M
import Data.Time.Clock

import Data.List (intersperse)

import qualified Test.Rufous.Options (RufousOptions(..))
import qualified Test.Rufous.Profile as P
import qualified Test.Rufous.DUG as D
--import qualified Test.Rufous.Run as R
import qualified Test.Rufous.Signature as S

import System.IO.Unsafe

type ImplementationTimes = M.Map String NominalDiffTime

printSummaryTable :: S.Signature -> [D.DUG] -> IO ()
printSummaryTable = undefined

select = undefined

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

-- Operation times for some DUG
-- todo: add dug + profile info to these ...
type OperationTimes = M.Map String OpRecord

select :: S.Signature -> [R.TimingDug a] -> IO ()
select sig dugs = T.time "(dbg) select" $ printSummaryTable sig dugs

padStr :: Char -> Int -> String -> String
padStr c n s = s ++ replicate (n - length s) c

printSummaryTable :: S.Signature -> [R.TimingDug a] -> IO ()
printSummaryTable s dugs = do
   let times = map (buildOpTimes s) dugs
   let table = makeSummaryTable s times

   let noRows = length table
   let noCols = length $ head table

   let dugNames = do {
      dug <- dugs;
      case dug ^. D.dugName of
         Just n -> return n
         Nothing -> return ""
      }

   let wrappedTable = insertCol ("dug name" : dugNames) table
   let lengths = maxLens wrappedTable

   let formatted = map (formatRow " | " wrappedTable) wrappedTable

   let headerLine = formatRow " + " wrappedTable [padStr '-' n "" | n <- lengths]
   putStrLn $ unlines (head formatted : headerLine : tail formatted)
   putStrLn ""

-- this takes a DUG and builds the count/total time for each operation for each implementation
printTable :: S.Signature -> R.TimingDug a -> IO ()
printTable s dug = do
   let times = (buildOpTimes s dug)
   let table = makeTable times
   let noRows = length table
   let noCols = length $ head table

   let dugName = do {
      case dug ^. D.dugName of
         Just n -> n
         Nothing -> ""
      }

   let wrappedTable = insertCol (dugName : replicate (noRows - 1) "") table
   let lengths = maxLens wrappedTable

   let formatted = map (formatRow " | " wrappedTable) wrappedTable

   let headerLine = formatRow " + " wrappedTable [padStr '-' n "" | n <- lengths]
   putStrLn $ unlines (head formatted : headerLine : tail formatted)
   putStrLn ""

insertCol :: [String] -> [[String]] -> [[String]]
insertCol [] [] = []
insertCol (rc:rcs) (r:rs) = (rc:r) : insertCol rcs rs

formatRow :: String -> [[String]] -> [String] -> String
formatRow sep table row = concat (intersperse sep padded)
   where lengths = maxLens table
         padded = map (\(i, s) -> padStr ' ' i s) (zip lengths row)

maxLens :: [[ [a] ]] -> [Int]
maxLens ([]:_) = []
maxLens xs = (maximum . map (length . head) $ xs) : maxLens (map tail xs)

makeTable :: OperationTimes -> [[String]]
makeTable times = header : map makeRow (M.elems times)
   where header = "operation" : "count" : opHeaders
         first = M.elems times !! 0
         opHeaders = addHeaders $ M.keys (first ^. implTotalTimes)
         addHeaders [] = []
         addHeaders (x:xs) = (x ++ " (total)") : (x ++ " (average)") : addHeaders xs

makeRow :: OpRecord -> [String]
makeRow r = 
   name : show count : concat (map (makeCols count) (M.elems implTimes))
   where
      name = r ^. opName
      count = r ^. opCount
      implTimes = r ^. implTotalTimes

makeCols :: Int -> NominalDiffTime -> [String]
makeCols count diffTime =
   [ show diffTime
   , if count > 0 then
      show (diffTime / (fromIntegral count))
     else
      show (0 :: NominalDiffTime)
   ]

-- [{empty: (empty, 0, {impl1: 0.5, impl2: 0.3}), ...}, ...]
makeSummaryTable :: S.Signature -> [OperationTimes] -> [[String]]
makeSummaryTable s (times@(t:ts)) = table
   where
      summaryRecords = makeSummaryRecords s times
      table = (header t) : map row summaryRecords
      impls = M.keys $ ((M.elems t) !! 0) ^. implTotalTimes
      header t = 
            [show o ++ " count" | o <- (M.keys t)] 
         ++ ["mortality", "pmf", "pof"] 
         ++ [show i ++ " time" | i <- impls]
      row sr = 
            [show o | o <- (M.elems (sr ^. ops))]
         ++ [show (sr ^. mortality), show (sr ^. pmf), show (sr ^. pof)] 
         ++ [show t | t <- (M.elems (sr ^. implTimes))]

makeSummaryRecords :: S.Signature -> [OperationTimes] -> [SummaryRecord]
makeSummaryRecords s times = map (makeSummaryRecord s) times

makeSummaryRecord :: S.Signature -> OperationTimes -> SummaryRecord
makeSummaryRecord s t = 
      SummaryRecord (first ^. opDug) cs m pmf pof implTimes
  where cs = getOpCounts (M.elems t)
        first = (M.elems t) !! 0
        p = 
         case first ^. opDug of 
            T a -> D.extractProfile s a
        m = p ^. P.mortality
        pmf = S.pmf s p
        pof = S.pof s p
        implTimes = getImplTimes (M.elems t)

getOpCounts :: [OpRecord] -> M.Map String Int
getOpCounts = go M.empty
   where
      go m [] = m
      go m (r:rs) = 
         let m' = M.insertWith (\_ c -> c + (r ^. opCount)) (r ^. opName) (r ^. opCount) m
         in go m' rs

getImplTimes :: [OpRecord] -> M.Map String NominalDiffTime
getImplTimes rs = foldr1 addMaps [r ^. implTotalTimes | r <- rs]

emptyOpTimes :: S.Signature -> R.TimingDug a -> OperationTimes
emptyOpTimes s d = M.fromList [(name o, OpRecord (name o) 0 (emptyImplTimes d) (T d)) | o <- s ^. S.operations & M.elems]
   where name o = o ^. S.opName

emptyImplTimes :: R.TimingDug a -> ImplementationTimes
emptyImplTimes d = M.fromList [(i ^. S.implName, 0) | i <- impls]
   where firstNode = (d ^.. D.operations . traverse . _1) !! 0
         impls = firstNode ^.. D.node . _2 . traverse . _1

buildOpTimes :: S.Signature -> R.TimingDug a -> OperationTimes
buildOpTimes s d = D.foldDug updateNode (emptyOpTimes s d) d

updateNode :: OperationTimes -> D.Node (a, R.TimingValue) -> OperationTimes
updateNode times n = M.adjust (\r -> r & opCount %~ succ & implTotalTimes %~ (\im -> foldr updateImpl im impls)) (n ^. D.nodeOperation ^. S.opName) times
   where (_, impls) = n ^. D.node

updateImpl :: (S.Implementation, NominalDiffTime) -> ImplementationTimes -> ImplementationTimes
updateImpl (impl, ndiff) = M.adjust (\d -> d + ndiff) (impl ^. S.implName)

-- assume k `elem` (keys m1) <==> k `elem` (keys m2)
addMaps :: (Ord k, Num a) => M.Map k a -> M.Map k a -> M.Map k a
addMaps m1 m2 = M.fromList [(k, (m1 M.! k) + (m2 M.! k)) | k <- M.keys m1]
-}
