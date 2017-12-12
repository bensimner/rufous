> module Test.Rufous.DUG where
> 
> import qualified Data.Map as M
> import Data.Maybe
> import Data.List
> import System.Process
> 
> import Test.Rufous.RndUtil
> import qualified Test.Rufous.Signature as S

Each node in the DUG is just the string of the operation name

> type VersionNode st = (String, [st], S.Operation st)

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

> observers :: S.Signature st -> DUG st -> [Int]
> observers = undefined

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
>       (name, _, _) = versions d !! ix
>       args2defn (a : as) (lambdaArgs, bodyArgs) =
>           case a of
>               VersionNodeArg _ -> 
>                   let vName = "v" ++ (show (length lambdaArgs)) 
>                   in args2defn as (lambdaArgs ++ [vName], bodyArgs ++ [vName])
>               NonVersionArg  k -> args2defn as (lambdaArgs, bodyArgs ++ [show k])
>       args2defn [] vs = vs
>       (lambdaArgs, bodyArgs) = args2defn args ([], [])
>       prefix = if null lambdaArgs then "" else "\\\\" ++ intercalate " " lambdaArgs ++ " -> "
