> {-# LANGUAGE BangPatterns #-}
> module Test.Rufous.Run where

> import Lens.Micro
> import Lens.Micro.TH
> import Data.Dynamic

> import Debug.Trace

> import Data.Time.Clock
> import Data.Maybe

> import qualified Data.Map as M

> import qualified Test.Rufous.DUG as D
> import qualified Test.Rufous.Signature as S
> import qualified Test.Rufous.Generate as G

> import qualified Test.Rufous.Internal.Timing as T

To "run" a DUG is simple:
    + given a DUG with a list of operations, create a list of versions

The output is an annotated DUG of timing values.

Each node then gets annotated with a timing result for each implementation:

> type TimingValue = [(S.Implementation, NominalDiffTime)]
> type TimingDug a = D.DUG (a, TimingValue)

To tag the nodes in the DUG:

> emptyImplDug :: D.DUG a -> TimingDug a
> emptyImplDug d = flip (,) [] <$> d

> tagDugTimes :: TimingDug a -> [(S.Implementation, NominalDiffTime)] -> TimingDug a
> tagDugTimes d times = d & D.operations .~ unpairs
>   where pairs = d ^. D.operations
>         pairs' = zip times pairs
>         unpairs = map unpair pairs'
>         unpair (t, n) = n & D.node %~ (\(a, b) -> (a, b ++ [t]))

> runDUG :: [S.Implementation] -> D.DUG a -> IO (TimingDug a)
> runDUG impls dug = T.time "(dbg) runDUG" $ updateDug impls (emptyImplDug dug)
>   where
>       versionNodes :: S.Implementation -> [(Dynamic, S.ImplType)]
>       versionNodes impl = versions
>           where 
>               versions = do
>                   (i, node) <- map (\x -> (x ^. D.nodeIndex, x)) (dug ^. D.operations)
>                   let opName = node ^. D.nodeOperation ^. S.opName
>                   let dynArgs = do
>                                 arg <- node ^. D.nodeArgs
>                                 case arg of
>                                     S.Version k    -> return $ ((versions !! k) & fst)
>                                     S.NonVersion (S.IntArg k) -> return $ toDyn k
>                                     S.NonVersion (S.BoolArg b) -> return $ toDyn b
>                                     S.NonVersion (S.VersionParam k) -> return $ toDyn (k :: Int)
>                   return $ G.runNode impl opName dynArgs
> 
>       -- the `t` is needed here to constrain the dynamic unwrap
>       -- todo: Extract the result from the dynamic cell
>       runVersion :: S.ImplType -> Dynamic -> IO NominalDiffTime
>       runVersion t d = do
>           let action = G.runDynamic t d
>           (_, !time) <- T.time "(dbg) runVersion" $ record action
>           return time
>       runAll :: [(Dynamic, S.ImplType)] -> IO [NominalDiffTime]
>       runAll [] = return []
>       runAll ((d,t):vs) = do
>           time <- runVersion t d
>           times <- runAll vs
>           return $ time : times
>       updateDug [] d = return d
>       updateDug (impl:impls) d = do
>           !times <- runAll (versionNodes impl)
>           let implTimes = map ((,) impl) times
>           updateDug impls (tagDugTimes d implTimes)

Time recording functions
------------------------

> record :: IO a -> IO (a, NominalDiffTime)
> record x = do
>    t1 <- getCurrentTime
>    y <- x
>    t2 <- getCurrentTime
>    let d = diffUTCTime t2 t1
>    return (y, d)

Time information extracting functions
-------------------------------------

> subDug :: [S.Implementation] -> TimingDug a -> TimingDug a 
> subDug impls d = subTimingValues impls <$> d

> subTimingValues :: [S.Implementation] -> (a, TimingValue) -> (a, TimingValue)
> subTimingValues impls t = t & _2 %~ filter f
>   where
>       f :: (S.Implementation, NominalDiffTime) -> Bool
>       f v = (v ^. _1) `elem` impls

> diffDugs :: TimingDug a -> TimingDug a -> TimingDug a
> diffDugs a b = newA
>   where times = b ^.. D.operations . traverse . D.node . _2 . ix 0 ^.. traverse . _2
>         zippedOps  = zip times (a ^. D.operations)
>         newA  = a & D.operations .~ map f zippedOps
>         f (t, n) = n & D.node . _2 . traverse . _2 %~ (\x -> x - t)

> -- remove overhead from null impl
> normaliseDug :: S.Signature -> TimingDug a -> TimingDug a
> normaliseDug s d = diffDugs remDug nullDug
>   where nullDug = subDug [s ^. S.nullImpl] d
>         remDug = subDug (dugImpls d) d

> dugImpls :: TimingDug a -> [S.Implementation]
> dugImpls d = firstNode ^. D.node ^. _2 ^.. traverse . _1 & tail  -- remove the first (the Null Impl)
>   where firstNode = (d ^. D.operations) !! 0

> -- extracts the total (wall-clock) time the DUG took to execute
> runTime :: TimingDug a -> NominalDiffTime
> runTime d = sum times
>   where times = [n ^. D.node & snd & map snd & sum | n <- d ^. D.operations]
