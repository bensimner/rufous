> module Test.Rufous.DUG where
> 
> import qualified Data.Map as M
> import Data.Maybe
> import Data.List
> import System.Process
> 
> import Test.Rufous.RndUtil
> import qualified Test.Rufous.Signature as S
> import qualified Test.Rufous.Profile as Pr

Each node in the DUG is just the string of the operation name

> data VersionNode st =
>   VersionNode 
>       { label :: String
>       , args  :: S.OpArgs st
>       , state :: st
>       , op    :: S.Operation st
>       }
> instance Show (VersionNode st) where
>   show (VersionNode name _ _ _) = "[" ++ name ++ "]"

Args can be either versiosn from other Nodes in the DUG or non-version arguments
Currently there is no way of representing a non-version argument from the DUG (such as the result of an observation)

> data Arg =
>      VersionNodeArg Int
>    | NonVersionArg  Int
>    deriving (Show)

Then the DUG is just a collection of nodes and a set of edges for each node.

> data DUG st =
>    DUG
>       { versions :: [VersionNode st]
>       , operations :: M.Map Int [Arg]
>       }
>    deriving (Show)

Extracting information from the DUG is easy with a few combinators

> observers :: S.Signature st -> DUG st -> [VersionNode st]
> observers s d = filter (not . isObs) (versions d)
>   where
>       isObs n = S.isType s S.Observer (op n)

Including extracting the full Profile from the DUG:

> toProfile :: S.Signature st -> DUG st -> Pr.Profile
> toProfile s d = 
>    Pr.Profile
>       { Pr.mutatorWeights = undefined
>       , Pr.observerWeights = undefined
>       , Pr.generatorWeights = undefined
>       , Pr.persistentMutationWeight = 0
>       , Pr.persistentObservationWeight  = 0
>       , Pr.mortality = 0
>       }
>   where
>       x = x

This representation allows easy displaying, such as a graphviz file:

> dug2dot :: DUG st -> IO ()
> dug2dot d = do
>   print "Writing .tmp.gv"
>   writeFile ".tmp.gv" ""
>   write "digraph G {"
>   write . unlines $ [show i ++ "[label=\"" ++ nodeLabel d i ++ "\"]" | (i, _) <- enumerate (versions d)]
>   write . unlines $ [show i ++ "->" ++ show j | (i, j) <- edges d]
>   write "}"
>   print "written"
>   createProcess (proc "dot" [".tmp.gv", "-Tpng", "-o", "tmp.png"])
>   print "made dot"
>   return ()
>       where
>           write s = appendFile ".tmp.gv" (s ++ "\n")
> 
> edges :: DUG st -> [(Int, Int)]
> edges d = concat $ map args2ints (M.toList (operations d))
>   where
>       args2ints :: (Int, [Arg]) -> [(Int, Int)]
>       args2ints (i, as) = mapMaybe (arg2int . ((,) i)) as
>       arg2int :: (Int, Arg) -> Maybe (Int, Int)
>       arg2int (i, a) =
>           case a of
>               VersionNodeArg j -> Just (j, i)
>               _                -> Nothing
> 
> nodeLabel :: DUG st -> Int -> String
> nodeLabel d ix = prefix ++ name ++ " " ++ intercalate " " bodyArgs
>   where
>       args :: [Arg]
>       args = operations d M.! ix
>       name :: String
>       name = label $ versions d !! ix
>       args2defn (a : as) (lambdaArgs, bodyArgs) =
>           case a of
>               VersionNodeArg _ -> 
>                   let vName = "v" ++ (show (length lambdaArgs)) 
>                   in args2defn as (lambdaArgs ++ [vName], bodyArgs ++ [vName])
>               NonVersionArg  k -> args2defn as (lambdaArgs, bodyArgs ++ [show k])
>       args2defn [] vs = vs
>       (lambdaArgs, bodyArgs) = args2defn args ([], [])
>       prefix = if null lambdaArgs then "" else "\\\\" ++ intercalate " " lambdaArgs ++ " -> "
