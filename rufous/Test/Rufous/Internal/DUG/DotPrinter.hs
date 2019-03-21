module Test.Rufous.Internal.DUG.DotPrinter where

import Control.Lens

import Data.List (intercalate)
import System.Process

import qualified Test.Rufous.Signature as S

import Test.Rufous.Internal.DUG.Types

nodeLabel :: Node -> String
nodeLabel n = lblId ++ ": " ++ opStr ++ " " ++ intercalate " " argStrs
   where lblId = show $ n ^. nodeId
         opStr = n ^. operation ^. S.opName
         argStrs = map show (n ^. args)

-- | Prints a DUG to file using DOT
printDUG :: String -> DUG -> IO ()
printDUG fName d = do
   putStrLn $ "[dot/...] writing dug " ++ fName ++ " ..."
   writeFile dotName ""
   write "digraph G {"
   write "overlap=\"false\""
   write . unlines $ [show (n ^. nodeId) ++ "[label=\"" ++ nodeLabel n ++ "\"]"  | n <- nodes d]
   write . unlines $ [show from ++ "->" ++ show to  | (to, from) <- edges d]
   write "}"
   createProcess (proc "neato" [dotName, "-Tpdf", "-o", pngName])
   return ()
   where write s = appendFile dotName (s ++ "\n")
         dotName = fName ++ ".dot"
         pngName = fName ++ ".pdf"
