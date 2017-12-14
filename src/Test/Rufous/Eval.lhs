The basic algorithm for evaluating a DUG is rather simple:

Given each implementation as a fulled-qualified module name, generate a haskell program that imports it
runs it, and returns timing information.

> module Test.Rufous.Eval where
> 
> import Data.List (intercalate)
> import qualified Data.Map as M
> 
> import GHC.IO.Handle
> import System.Process
> import System.Exit
> 
> import qualified Data.String as Str
> import qualified Data.ByteString.Lazy as BL
> import Data.Aeson
> import Data.Time.Clock
> 
> import Test.Rufous.RndUtil
> import qualified Test.Rufous.Signature as S
> import qualified Test.Rufous.DUG as D

The first thing that's needed is some representation of the "result" of running a DUG.
There is timing information to contend with of course, but there is also capturing the result.

This is simple for non-version arguments as they have simple fixed representations with Eq instances (Int)
but for version arguments this may not be possible, so we can only observe the "observation" operations.

Profiling a DUG is relatively simple, taking an outline for a file that runs a DUG:
    - One instance for the implementation under test
    - definitions for each node in the DUG
    - Then Haskell evaluates this file, timing the evaluation and collecting the results of each observer
    - This information is then written to a test json file that can be read by the parent process

> dugTemplate :: String
> dugTemplate = unlines $
>     [ "{-# LANGUAGE OverloadedStrings #-}"
>     , "module Test{} where"
>     , ""
>     , "import Data.Aeson"
>     , "import qualified Data.ByteString.Lazy as B"
>     , "import Data.Time.Clock"
>     , ""
>     , "import qualified {}"
>     , ""
>     , "{}"
>     , ""
>     , "-- Haskell-ish DUG"
>     , "-- each v-node has type either T a or just a"
>     , "{}"
>     , ""
>     , "{}"
>     , ""
>     , "-- Recording"
>     , "record :: IO a -> IO (a, NominalDiffTime)"
>     , "record x = do"
>     , "   t1 <- getCurrentTime"
>     , "   y <- x"
>     , "   t2 <- getCurrentTime"
>     , "   let d = diffUTCTime t2 t1"
>     , "   return (y, d)"
>     , ""
>     , "collectObservers :: IO ([Int], NominalDiffTime)"
>     , "collectObservers = record $ sequence $ map return [{}]"
>     , ""
>     , "run :: a -> IO ()"
>     , "run x = x `seq` return ()"
>     , ""
>     , "collectOthers :: IO NominalDiffTime"
>     , "collectOthers = do"
>     , "   (_, t) <- record $ sequence $ map run [{}]"
>     , "   return t"
>     , ""
>     , "collectAll :: IO TimingResult"
>     , "collectAll = do"
>     , "   (res, t1) <- collectObservers"
>     , "   t2 <- collectOthers"
>     , "   return $ TimingResult (res, t1 + t2)"
>     , ""
>     , "newtype TimingResult = TimingResult ([Int], NominalDiffTime) deriving (Show)"
>     , "instance ToJSON TimingResult where"
>     , "   toJSON (TimingResult (r, t)) = object [\"time\" .= toJSON t, \"result\" .= toJSON r]"
>     , ""
>     , "main = do"
>     , "  t <- collectAll"
>     , "  B.writeFile \"{}\" (encode t)"
>     ]

To fill this template in, we need:
    - A name for the module/file
    - The fully qualified module for import
    - implementation fully qualified type name
    - implementation instance definitions (bodies)
    - DUG
    - generator type annotations

> fillTemplate moduleName modulePath implDefn dugCode dugAnnotations obs muts fileName =
>   dugTemplate 
>       `replace` moduleName 
>       `replace` modulePath 
>       `replace` implDefn 
>       `replace` dugAnnotations 
>       `replace` dugCode 
>       `replace` obs 
>       `replace` muts 
>       `replace` fileName 
> 
> replace :: String -> String -> String
> replace ('{' : '}' : ss) sub = sub ++ ss
> replace (s : ss) sub = s : replace ss sub
> replace [] sub = error "replace :: could not find pattern"
> 
> names :: D.DUG st -> [(String,  D.VersionNode st)]
> names d = [("v" ++ (show i), v) | (i, v) <- enumerate (D.versions d)]
> 
> generateDUGCode :: D.DUG st -> String
> generateDUGCode d = unlines [line ix | ix <- [0 .. (length vs) - 1]]
>   where
>       vs = D.versions d
>       ops = D.operations d
>       opname ix = let o = D.op (vs !! ix) in S.opName o
>       line ix = intercalate " " $ (name ix) : "=" : opname ix : args ix
>       name ix = "v" ++ (show ix)
>       args ix = map arg2str (ops M.! ix)
>       arg2str a =
>           case a of
>               D.VersionNodeArg v -> name v
>               D.NonVersionArg i -> show i
> 
> -- this generates the signature for the generators that are used
> generateDUGSignature :: S.Signature st -> D.DUG st -> String -> String
> generateDUGSignature s d tyName = unlines (map line vs)
>   where
>       vs = filter (\(_, n) -> S.isType s S.Generator (D.op n)) (names d)
>       line (name, _) = name ++ " :: " ++ tyName ++ " Int"
>
> generateADTImpl :: S.Signature st -> S.Implementation -> String
> generateADTImpl s (tyName, code) = unlines $ map line ops
>   where
>       ops = M.elems $ S.operations s
>       line o = 
>           let name = S.opName o 
>           in unlines $ [ name ++ " :: " ++ S.sig2str tyName "Int" (S.sig o)
>                        , name ++ " = " ++ (code M.! name)
>                        ]
> 
> indent :: String -> String
> indent = unlines . map indentLine . lines
>   where
>       indentLine xs = "   " ++ xs
> 
> observers :: S.Signature st -> D.DUG st -> String
> observers s d = intercalate ", " obs
>   where
>       vs = names d
>       obsNames = map S.opName (S.observers s)
>       obs = map fst $ filter (\(_, n) -> (S.opName $ D.op n) `elem` obsNames) vs
>
> mutators :: S.Signature st -> D.DUG st -> String
> mutators s d = intercalate ", " muts
>   where
>       vs = names d
>       mutNames = map S.opName (S.mutators s)
>       muts = map fst $ filter (\(_, n) -> (S.opName $ D.op n) `elem` mutNames) vs
> 
> generateDUGFile :: S.Signature st -> D.DUG st -> S.Implementation -> String -> String
> generateDUGFile s d (impl@(tyName, code)) outFileName = 
>   fillTemplate 
>       (S.nameFromType tyName)       
>       (S.moduleFromType tyName) 
>       (generateADTImpl s impl)
>       (generateDUGCode d)
>       (generateDUGSignature s d tyName)
>       (observers s d)
>       (mutators s d)
>       (outFileName)
>
> type TimingInfo   = NominalDiffTime
> data TimingResult = TimingResult [Int] TimingInfo deriving (Show)
> instance ToJSON TimingResult where
>    toJSON (TimingResult r t) = object [(Str.fromString "time") .= toJSON t, (Str.fromString "result") .= toJSON r]
> instance FromJSON TimingResult where
>    parseJSON (Object v) = TimingResult <$> v .: (Str.fromString "result") <*> v.: (Str.fromString "time")
> 
> writeDUGFile :: S.Signature st -> D.DUG st -> S.Implementation -> IO (String, String)
> writeDUGFile s d (impl@(tyName, _)) = do
>   let fileName = "dugs/" ++ (S.nameFromType tyName) ++ "_input.hs"
>   let outputFileName = "dugs/" ++ (S.nameFromType tyName) ++ "_output.json"
>   let fileContents = generateDUGFile s d impl outputFileName
>   writeFile fileName fileContents
>   return (fileName, outputFileName)
>
> readResults :: String -> IO (Either String TimingResult)
> readResults f = do
>   fContents <- BL.readFile f
>   case (decode fContents) of
>       Just a -> return $ Right a
>       Nothing -> return $ Left $ "Failed to load: " ++ f
>   
> 
> runDUG :: S.Signature st -> D.DUG st -> S.Implementation -> IO (Either String TimingResult)
> runDUG s d impl = do
>   (fileName, outFileName) <- writeDUGFile s d impl
>   (_, _, Just herr, ph) <- createProcess (proc "runhaskell" [fileName]) {std_err = CreatePipe}
>   ec <- waitForProcess ph
>   case ec of
>       ExitFailure _ -> Left <$> hGetContents herr
>       ExitSuccess   -> readResults outFileName
> 
> validateDUG :: S.Signature st -> D.DUG st -> TimingResult -> Either String TimingInfo
> validateDUG s d (TimingResult obsResults ti) = if allValid then Right ti else Left "Post-condition failure"
>   where
>       obsNodes = D.observers s d
>       validNode n i = S.post (D.op n) (D.args n) i
>       allValid = all (uncurry validNode) (zip obsNodes obsResults)
>
> tryEvaluateDUG :: S.Signature st -> D.DUG st -> S.Implementation -> IO (Either String TimingInfo)
> tryEvaluateDUG s d impl = do
>   r <- runDUG s d impl 
>   return $ r >>= validateDUG s d 
