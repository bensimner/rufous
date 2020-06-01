module Test.Rufous.Internal.DUG.HsPrinter where

import System.FilePath
import System.Directory
import Control.Lens

import qualified Test.Rufous.Signature as S

import Test.Rufous.Internal.DUG.Types
import Data.List (sortOn)

sprintDUG :: DUG -> String
sprintDUG d = unlines defns
   where defns = sortOn head [defn n | n<-nodes d, n^.nodeId >= 0]

defn :: Node -> String
defn n = unwords [varName, "=", opName, body]
   where varName =
            if (n^.operation^.S.opCategory) == S.Observer then
               "o" ++ show (n^.nodeId)
            else
               "v" ++ show (n^.nodeId)
         opName  = n^.operation^.S.opName
         body = unwords [sarg a | a <- n^.args]
         sarg (S.Version (-1)) = "undefined"
         sarg (S.Version k) = "v" ++ show k
         sarg (S.NonVersion (S.VersionParam k)) = show k
         sarg (S.NonVersion (S.ArbArg k _ _)) = show k

checkPath :: FilePath -> IO ()
checkPath pth = do
   let dir = takeDirectory pth
   exists <- doesDirectoryExist dir
   if exists then
      return ()
   else
      error $ "Failed to render DUG. Directory " ++ dir ++ " does not exist."

printDUGtoFile :: FilePath -> DUG -> IO ()
printDUGtoFile fName d = checkPath fName >> writeFile hsName (sprintDUG d)
   where hsName = addExtension fName "hs"

printDUG :: FilePath -> DUG -> IO ()
printDUG dir d = do
   let rootFName = dir </> (d^.name)
   printDUGtoFile rootFName d
