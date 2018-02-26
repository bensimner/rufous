module Test.Rufous.Select where

import Control.Lens ((^.), (&), _3)

import qualified Data.Map as M
import Data.Time.Clock

import Data.List (intersperse)

import Test.Rufous.Profile as P
import Test.Rufous.DUG as D
import Test.Rufous.Run as R
import Test.Rufous.Signature as S

select :: S.Signature -> [R.TimingDug a] -> IO ()
select sig = mapM_ (printTable sig)

padStr :: Char -> Int -> String -> String
padStr c n s = s ++ replicate (n - length s) c

-- this takes a DUG and builds the count/total time for each operation for each implementation
printTable :: S.Signature -> R.TimingDug a -> IO ()
printTable s dug = do
   let table = makeTable (buildOpTimes s dug)
   let lengths = maxLens table
   let formatted = map (formatRow " | " table) table
   let headerLine = formatRow " + " table [padStr '-' n "" | n <- lengths]
   let lineLength = length $ head formatted
   case dug ^. D.dugName of
      Just n -> putStrLn n
      Nothing -> return ()
   putStrLn $ unlines (head formatted : headerLine : tail formatted)
   putStrLn ""

formatRow :: String -> [[String]] -> [String] -> String
formatRow sep table row = concat (intersperse sep padded)
   where lengths = maxLens table
         padded = map (\(i, s) -> padStr ' ' i s) (zip lengths row)

maxLens :: [[ [a] ]] -> [Int]
maxLens ([]:_) = []
maxLens xs = (maximum . map (length . head) $ xs) : maxLens (map tail xs)

type Row = (String, Int, ImplementationTimes)

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

type ImplementationTimes = M.Map String NominalDiffTime
type OperationTimes = M.Map String Row

emptyOpTimes :: S.Signature -> R.TimingDug a -> OperationTimes
emptyOpTimes s d = M.fromList [(name o, (name o, 0, emptyImplTimes d)) | o <- s ^. S.operations & M.elems]
   where name o = o ^. S.opName

emptyImplTimes :: R.TimingDug a -> ImplementationTimes
emptyImplTimes d = M.fromList [(i ^. S.implName, 0) | i <- impls]
   where n = (d ^. D.operations) !! 0
         impls = map fst (snd (n ^. node))

buildOpTimes :: S.Signature -> R.TimingDug a -> OperationTimes
buildOpTimes s d = D.foldDug updateNode (emptyOpTimes s d) d

updateNode :: D.Node (a, R.TimingValue) -> OperationTimes -> OperationTimes
updateNode n = M.adjust (\(s, c, im) -> (s, c+1, foldr updateImpl im impls)) (n ^. D.nodeOperation ^. S.opName)
   where (_, impls) = n ^. D.node

updateImpl :: (S.Implementation, NominalDiffTime) -> ImplementationTimes -> ImplementationTimes
updateImpl (impl, ndiff) = M.adjust (\d -> d + ndiff) (impl ^. S.implName)
