module Test.Rufous.Internal.DUG.DotPrinter where

import System.FilePath
import System.Directory
import Control.Lens ((^.))

import Data.Maybe (fromJust)
import Data.List (intercalate)
import System.Process

import qualified Test.Rufous.Options as Opt
import qualified Test.Rufous.Signature as S
import qualified Test.Rufous.Run as R

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

showNodeDetail :: S.Signature -> Node -> String
showNodeDetail s n = show (n ^. nodeId) ++ "[label=\"" ++ nodeLabel n ++ " :: (" ++ shadowStr ++ ")\"]"
   where
      shadowStr =
         case n^.shadow of
            Just d -> R.nodeShow (n^.operation) (fromJust (s^.S.shadowImpl)) d
            _ -> "N/A"

checkPath :: FilePath -> IO ()
checkPath pth = do
   let dir = takeDirectory pth
   exists <- doesDirectoryExist dir
   if exists then
      return ()
   else
      error $ "Failed to render DUG. Directory " ++ dir ++ " does not exist."

-- | Prints a DUG to file using DOT
printDUGtoFile :: Opt.RufousOptions -> FilePath -> DUG -> IO String
printDUGtoFile opts rootFName d = do
   checkPath rootFName
   writeFile dotName ""
   write "digraph G {"
   write "overlap=\"false\""
   if hasUndefined d then
      write "-1 [label=\"undefined\"]"
   else
      return ()
   write . unlines $ [showN n | n <- nodes d]
   write . unlines $ [showEdge to arc from | (to, arc, from) <- edges d]
   write "}"
   _ <- createProcess (proc "neato" [dotName, "-Tpdf", "-o", pdfName])
   return pdfName
   where write s = appendFile dotName (s ++ "\n")
         dotName = addExtension rootFName "dot"
         pdfName = addExtension rootFName "pdf"
         showN = case Opt.dumpDUGDetail (Opt.outputOptions opts) of
            1 -> showNode
            2 -> showNodeDetail (Opt.signature opts)
            _ -> error "Rufous: unexpected dumpDUGDetail, expect one of {1,2}"

printDUG :: Opt.RufousOptions -> FilePath -> DUG -> IO ()
printDUG opts dir d = do
   let rootFName = dir </> (d^.name)
   _ <- printDUGtoFile opts rootFName d
   return ()
