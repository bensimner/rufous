module Test.Rufous.Internal.DUG.HsPrinter where

import System.FilePath
import System.Directory
import Control.Lens

import qualified Test.Rufous.Signature as S
import qualified Test.Rufous.Options as Opt

import Test.Rufous.Internal.DUG.Types
import Data.List (sortOn)

sprintDUG :: DUG -> String
sprintDUG d = unlines defns
   where defns = sortOn head [showNode n | n<-nodes d, n^.nodeId >= 0]

showArg :: DUGArg -> String
showArg (S.Version (-1)) = "undefined"
showArg (S.Version k) = "v" ++ show k
showArg (S.NonVersion (S.VersionParam k)) = show k
showArg (S.NonVersion (S.ArbArg k _ _)) = show k

showVarName :: Node -> String
showVarName n =
   if (n^.operation^.S.opCategory) == S.Observer
      then "o" ++ show (n^.nodeId)
      else "v" ++ show (n^.nodeId)

showNode :: Node -> String
showNode n = unwords [showVarName n, "=", opName, body]
   where opName  = n^.operation^.S.opName
         body = unwords [showArg a | a <- n^.args]

checkPath :: FilePath -> IO ()
checkPath pth = do
   let dir = takeDirectory pth
   exists <- doesDirectoryExist dir
   if exists then
      return ()
   else
      error $ "Failed to render DUG. Directory " ++ dir ++ " does not exist."

printDUGtoFile :: Opt.RufousOptions -> FilePath -> DUG -> IO String
printDUGtoFile opts fName d = do
      checkPath fName >> writeFile hsName (sprintDUG d)
      return hsName
   where hsName = addExtension fName "hs"

printDUG :: Opt.RufousOptions -> FilePath -> DUG -> IO ()
printDUG opts dir d = do
   let rootFName = dir </> (d^.name)
   _ <- printDUGtoFile opts rootFName d
   return ()