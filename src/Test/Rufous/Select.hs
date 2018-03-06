module Test.Rufous.Select where

import Lens.Micro ((^.), (&), _1, _2, (^..), _3)

import qualified Data.Map as M
import Data.Time.Clock

import Data.List (intersperse)

import Test.Rufous.Options (RufousOptions(..))
import Test.Rufous.Profile as P
import Test.Rufous.DUG as D
import Test.Rufous.Run as R
import Test.Rufous.Signature as S

type OperationName = String
type ImplementationName = String
type OperationCount = Int
type ImplementationTimes = M.Map ImplementationName NominalDiffTime
type Row = (OperationName, OperationCount, ImplementationTimes)

type OperationTimes = M.Map OperationName Row
type SummaryTimes = M.Map OperationName Row

{- 
 - summary:
 -    | empty count | snoc count | head count | tail count | mortality | pmf | pof | impl1 | impl2 | impl3
 -    | 5           | 6          | 3          | 2          | 0.6       | 0.2 | 0.3 | 12s   | 4s    | 12s  
 -}

select :: S.Signature -> [R.TimingDug a] -> IO ()
select sig dugs = printSummaryTable sig dugs

padStr :: Char -> Int -> String -> String
padStr c n s = s ++ replicate (n - length s) c

printSummaryTable :: S.Signature -> [R.TimingDug a] -> IO ()
printSummaryTable s dugs = do
   let times = map (buildOpTimes s) dugs
   let table = makeSummaryTable times

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
         opHeaders = addHeaders $ M.keys (((M.elems times) !! 0) ^. _3)
         addHeaders [] = []
         addHeaders (x:xs) = (x ++ " (total)") : (x ++ " (average)") : addHeaders xs

makeRow :: Row -> [String]
makeRow (name, count, implTimes) = name : show count : concat (map (makeCols count) (M.elems implTimes))

makeCols :: Int -> NominalDiffTime -> [String]
makeCols count diffTime =
   [ show diffTime
   , if count > 0 then
      show (diffTime / (fromIntegral count))
     else
      show (0 :: NominalDiffTime)
   ]

-- [{empty: (empty, 0, {impl1: 0.5, impl2: 0.3}), ...}, ...]
makeSummaryTable :: [OperationTimes] -> [[String]]
makeSummaryTable (times@(t:ts)) = table
   where
      table = (header t) : makeRows times
      makeRows [] = []
      makeRows (rt:rts) = row rt : makeRows rts
      header t = 
            [show o ++ " count" | o <- (M.keys t)] 
         ++ ["mortality"] 
         ++ [show i ++ " time (s)" | i <- (M.keys t)]
      row t = 
            [show o ++ " count" | o <- (M.keys t)]
         ++ ["mortality"] 
         ++ [show i ++ " time (s)" | i <- (M.keys t)]

emptyOpTimes :: S.Signature -> R.TimingDug a -> OperationTimes
emptyOpTimes s d = M.fromList [(name o, (name o, 0, emptyImplTimes d)) | o <- s ^. S.operations & M.elems]
   where name o = o ^. S.opName

emptyImplTimes :: R.TimingDug a -> ImplementationTimes
emptyImplTimes d = M.fromList [(i ^. S.implName, 0) | i <- impls]
   where firstNode = (d ^. D.operations) !! 0
         impls = firstNode ^.. D.node . _2 . traverse . _1

buildOpTimes :: S.Signature -> R.TimingDug a -> OperationTimes
buildOpTimes s d = D.foldDug updateNode (emptyOpTimes s d) d

updateNode :: OperationTimes -> D.Node (a, R.TimingValue) -> OperationTimes
updateNode times n = M.adjust (\(s, c, im) -> (s, c+1, foldr updateImpl im impls)) (n ^. D.nodeOperation ^. S.opName) times
   where (_, impls) = n ^. D.node

updateImpl :: (S.Implementation, NominalDiffTime) -> ImplementationTimes -> ImplementationTimes
updateImpl (impl, ndiff) = M.adjust (\d -> d + ndiff) (impl ^. S.implName)
