> {-# LANGUAGE TemplateHaskell #-}
> module Test.Rufous.DUG where
>
> import Control.Lens (makeLenses, (^.), (%~), (&))
>
> import qualified Data.Map as M
> import System.Process
> import Data.List (intercalate)
> 
> import Test.Rufous.Random
> import qualified Test.Rufous.Signature as S

The core type that Rufous deals with is the Datatype-Usage-Graph (DUG).
Control flow through rufous is typically transformations of and between DUG types.

Fundamentally a DUG is simply a directed-acyclic-graph where nodes represent results of applications
and edges are the arguments.

The usual node/edge types are simple:

> type DUGArg = S.Arg Int Int
> data Node n =
>   Node 
>       { _nodeOperation :: S.Operation
>       , _nodeArgs      :: [DUGArg]
>       , _nodeIndex     :: Int
>       , _node          :: n
>       }
>   deriving (Show)
> makeLenses ''Node

> data Edge e =
>   Edge 
>      { _from :: Int
>      , _to  :: Int
>      , _edge :: e
>      }
>   deriving (Show)
> makeLenses ''Edge

> data DUG n e =
>   DUG 
>      { _operations :: [Node n]
>      ,  _arguments :: M.Map Int [Edge e]
>      }
>   deriving (Show)
> makeLenses ''DUG

Initialisation and creation:

> emptyDug :: DUG n e
> emptyDug = DUG [] M.empty
>
> insertEdge :: Int -> Int -> e -> DUG n e -> DUG n e
> insertEdge i j e (d @ (DUG o a)) = d & arguments %~ (M.insert i edges')
>   where
>       edges = a M.! i
>       edge = Edge i j e
>       edges' = edges ++ [edge]
>
> insertOp :: Node n -> DUG n e -> DUG n e
> insertOp node d = d & operations %~ (++ [node])
>                     & arguments %~ (M.insert (node ^. nodeIndex) [])
> 
> generateNode :: S.Operation -> [DUGArg] -> DUG n e -> n -> Node n
> generateNode op args d n = Node op args ni n
>   where
>       ni = d ^. operations & length

There are many things one would like to extract from a DUG, which are easy with simple combinators:

> edges :: DUG n e -> [Edge e]
> edges d = concat $ M.elems (d ^. arguments)

> successors :: DUG n e -> Int -> [Edge e]
> successors d i = (d ^. arguments) M.! i

> predecessors :: DUG n e -> Int -> [Edge e]
> predecessors d i = concat $ map (filter (\e -> i == e ^. to)) (M.elems (d ^. arguments))

Debugging
---------

For debugging a pretty-printing function is defined:

> pprintDUG :: DUG n e -> String 
> pprintDUG d = lined ["Operations:", operationsRepr, "Arguments:", argumentsRepr]
>   where
>       operationsRepr = lined [pprintNode n | n <- d ^. operations]
>       argumentsRepr  = lined ["n" ++ show n ++ " -> " ++ show (map edgesRepr es) | (n, es) <- d ^. arguments & M.toList]
>       edgesRepr e = show (e ^. to)
>       lined = unlines . map (" + " ++)

> pprintNode :: Node n -> String
> pprintNode n = node ++ "=" ++ name ++ " " ++ args
>   where
>       node = "v" ++ (n ^. nodeIndex & show)
>       name = n ^. nodeOperation ^. S.opName
>       args = intercalate " " (map pprintDArg (n ^. nodeArgs))

> pprintEdge :: Edge e -> String
> pprintEdge e = ""

> pprintDArg :: DUGArg -> String 
> pprintDArg (S.Version i) = "v" ++ show i
> pprintDArg (S.NonVersion i) = show i

and conversion to GraphViz:

> dug2dot :: DUG n e -> String -> IO ()
> dug2dot d fName = do
>   print "[dot/...] making dot..."
>   writeFile dotName ""
>   write "digraph G {"
>   print "[dot/...] writing nodes..."
>   write . unlines $ [show i ++ "[label=\"" ++ pprintNode n ++ "\"]"  | (i, n) <- zip [0..] (d ^. operations)]
>   print "[dot/...] writing edges..."
>   write . unlines $ [show (e ^. from) ++ "->" ++ show (e ^. to) ++ "[label=\"" ++ pprintEdge e ++ "\"]"  | e <- edges d]
>   write "}"
>   print "[dot/...] written dot, compiling to png..."
>   createProcess (proc "dot" [dotName, "-Tpng", "-o", pngName])
>   print "[dot/done]"
>   where
>       write s = appendFile dotName (s ++ "\n")
>       dotName = fName ++ ".dot"
>       pngName = fName ++ ".png"
