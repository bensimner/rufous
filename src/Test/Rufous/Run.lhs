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

> runDUG :: [S.Implementation] -> D.DUG a -> IO (TimingDug a)
> runDUG impls dug = T.time "(dbg) runDUG" $ combineDugs $ map (runAll dug) impls
>   where
>       -- the `t` is needed here to constrain the dynamic unwrap
>       -- todo: Extract the result from the dynamic cell
>       runNode impl (a, (d, t)) = do
>           let action = G.runDynamic t d
>           (_, !time) <- record action
>           return $ (a, [(impl, time)])
>       
>       runAll :: D.DUG a -> S.Implementation -> (D.DUG (IO (a, TimingValue)))
>       runAll dug impl = (runNode impl) <$> (versionDug impl dug)

> combineDugs :: [D.DUG (IO (a, TimingValue))] -> IO (TimingDug a)
> combineDugs ds = do
>   ds' <- sequence $ map D.sequenceDugIO ds
>   return $ comb ds'
> comb [d] = d
> comb (d:d':ds) = comb (((f <$> d) <*> d') : ds)
>   where f (a, ts) (b, ts') = (a, ts ++ ts')

> versionDug :: S.Implementation -> D.DUG a -> D.DUG (a, (Dynamic, S.ImplType))
> versionDug impl dug = vdug
>     where 
>         vdug = D.mapDug node2dyn dug
>         node2dyn :: D.Node a -> D.Node (a, (Dynamic, S.ImplType))
>         node2dyn n = n & D.node %~ (\v -> (v, G.runNode impl opName dynArgs))
>             where
>                 opName = n ^. D.nodeOperation ^. S.opName
>                 dynArgs = do
>                     arg <- n ^. D.nodeArgs
>                     case arg of
>                         S.Version k    -> return $ ((vdug ^. D.operations) M.! k) ^. _1 . D.node . _2 . _1
>                         S.NonVersion (S.IntArg k) -> return $ toDyn k
>                         S.NonVersion (S.BoolArg b) -> return $ toDyn b
>                         S.NonVersion (S.VersionParam k) -> return $ toDyn (k :: Int)

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

> diffNullDug :: TimingDug a -> TimingDug a -> TimingDug a
> diffNullDug a n = ((\(_, tvx) (v, tvy) -> (v, tvx `tvDiff` tvy)) <$> a) <*> n
>   where
>       tvDiff tv [(_, d)] = [(i, y - d) | (i, y) <- tv]

> -- remove overhead from null impl
> normaliseDug :: S.Signature -> TimingDug a -> TimingDug a
> normaliseDug s d = remDug `diffNullDug` nullDug
>   where nullDug = subDug [s ^. S.nullImpl] d
>         remDug = subDug (dugImpls d) d

> dugImpls :: TimingDug a -> [S.Implementation]
> dugImpls d = firstNode ^. D.node ^. _2 ^.. traverse . _1 & tail  -- remove the first (the Null Impl)
>   where firstNode = ((d ^. D.operations) M.! 0) ^. _1

> -- extracts the total (wall-clock) time the DUG took to execute
> runTime :: TimingDug a -> NominalDiffTime
> runTime d = sum times
>   where times = [n ^. _1 . D.node . _2 & map snd & sum | n <- M.elems (d ^. D.operations)]
