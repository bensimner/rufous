module Test.Rufous.Internal.DUG.DotPrinter where

import System.FilePath
import System.Directory
import Control.Lens ((^.))

import Data.List (intercalate)
import System.Process

import qualified Test.Rufous.Signature as S

import Test.Rufous.Internal.DUG.Types

nodeLabel :: Node -> String
nodeLabel n = lblId ++ ": " ++ opStr ++ " " ++ intercalate " " argStrs
   where lblId = show $ n ^. nodeId
         opStr = n ^. operation ^. S.opName
         argStrs = map show (n ^. args)

hasUndefined :: DUG -> Bool
hasUndefined d = not $ null [() | (to, _, _) <- edges d, to == -1]

showEdge :: Int -> Int -> Int -> String
showEdge to arc from = show from ++ "->" ++ show to ++ "[label=\"" ++ show arc ++ "\"]"

showNode :: Node -> String
showNode n = show (n ^. nodeId) ++ "[label=\"" ++ nodeLabel n ++ "\"]"

checkPath :: FilePath -> IO ()
checkPath pth = do
   let dir = takeDirectory pth
   exists <- doesDirectoryExist dir
   if exists then
      return ()
   else
      error $ "Failed to render DUG. Directory " ++ dir ++ " does not exist."

-- | Prints a DUG to file using DOT
printDUGtoFile :: FilePath -> DUG -> IO ()
printDUGtoFile rootFName d = do
   checkPath rootFName
   putStrLn $ "[dot/...] writing dug " ++ dotName
   writeFile dotName ""
   write "digraph G {"
   write "overlap=\"false\""
   if hasUndefined d then
      write "-1 [label=\"undefined\"]"
   else
      return ()
   write . unlines $ [showNode n | n <- nodes d]
   write . unlines $ [showEdge to arc from | (to, arc, from) <- edges d]
   write "}"
   putStrLn $ "[dot/...] creating " ++ pdfName
   _ <- createProcess (proc "neato" [dotName, "-Tpdf", "-o", pdfName])
   return ()
   where write s = appendFile dotName (s ++ "\n")
         dotName = addExtension rootFName "dot"
         pdfName = addExtension rootFName "pdf"

printDUG :: FilePath -> DUG -> IO ()
printDUG dir d = do
   let rootFName = dir </> (d^.name)
   printDUGtoFile rootFName d
