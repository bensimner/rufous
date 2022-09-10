module Test.Rufous.DUG
   ( module Test.Rufous.Internal.DUG.Types
   , module Test.Rufous.Internal.DUG.ProfileExtractor
   , printDUGtoFile
   , printDUG
   , haskellSource
   )
where

import Test.Rufous.Internal.DUG.Types
import qualified Test.Rufous.Internal.DUG.DotPrint as DotPrint
import qualified Test.Rufous.Internal.DUG.HsPrint as HsPrint
import Test.Rufous.Internal.DUG.ProfileExtractor

import qualified Test.Rufous.Options as Opt

printDUGtoFile :: Opt.RufousOptions -> FilePath -> DUG -> IO String
printDUGtoFile = DotPrint.printDUGtoFile

printDUG :: Opt.RufousOptions -> FilePath -> DUG -> IO ()
printDUG = DotPrint.printDUG

-- | Print DUG as Haskell program
haskellSource :: DUG -> String
haskellSource = HsPrint.sprintDUG