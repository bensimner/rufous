> module Test.Rufous.Run where

> import Control.Lens
> import Data.Dynamic

> import Data.Time.Clock
> import Data.Maybe

> import qualified Data.Map as M

> import qualified Test.Rufous.DUG as D
> import qualified Test.Rufous.Signature as S
> import qualified Test.Rufous.Generate as G

To "run" a DUG is simple:
    + given a DUG with a list of operations, create a list of versions

The output is a TimingResult for that DUG,
 
> type TimingValues = [NominalDiffTime]

> runDUG :: S.Implementation -> G.GenDUG -> IO TimingValues
> runDUG impl dug = runAll versionActions
>   where
>       versionActions :: [(Dynamic, S.ImplType)]
>       versionActions = do
>           (i, node) <- zip [0..] $ dug ^. D.operations
>           let opName = node ^. G.nodeOperation ^. S.opName
>           let dynArgs = do
>               arg <- node ^. G.nodeArgs
>               case arg of
>                   S.Version i    -> return $ ((versionActions !! i) & fst)
>                   S.NonVersion k -> return $ toDyn (k :: Int)
>           return $ G.runNode impl opName dynArgs
> 
>       -- the `t` is needed here to constrain the dynamic unwrap
>       -- todo: Extract the result from the dynamic cell
>       runVersion :: Typeable t => t -> Dynamic -> (Dynamic -> Maybe t) -> IO NominalDiffTime
>       runVersion t d f = do
>           let action = (fromJust (f d)) `seq` return ()
>           ((), time) <- record action
>           return time
>       runAll :: [(Dynamic, S.ImplType)] -> IO [NominalDiffTime]
>       runAll [] = return []
>       runAll ((d,S.ImplType t):vs) = do
>           time <- runVersion t d fromDynamic
>           times <- runAll vs
>           return $ time : times

Time recording functions
------------------------

> record :: IO a -> IO (a, NominalDiffTime)
> record x = do
>    t1 <- getCurrentTime
>    y <- x
>    t2 <- getCurrentTime
>    let d = diffUTCTime t2 t1
>    return (y, d)

Example:
operations = M.fromList [("snoc", toDyn (snoc :: Int -> [Int] -> [Int])), ("empty", toDyn (empty :: [Int]))]

dyns :: [Dynamic]
dyns = [operations M.! "empty"]

data Ty = forall t. Typeable t => Ty { _typ :: t }

versionTy :: Ty
versionTy = Ty (undefined :: [Int])

-- Tries to run `snoc 3 (VERSION 0)`
run :: [Dynamic] -> [Dynamic]
run ds = ds ++ [(operations M.! "snoc") `dynApp` (args !! 0) `dynApp` (args !! 1)]
   where
      args = [toDyn (3 :: Int), ds !! 0]

evalD :: Ty -> Dynamic -> Float
evalD (Ty t) d = (ev t fromDynamic) `seq` 3.0
   where
      ev :: Typeable t => t -> (Dynamic -> Maybe t) -> ()
      ev t f = (f d) `seq` ()
