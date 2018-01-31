> {-# LANGUAGE TemplateHaskell #-}
> module Test.Rufous.DUG where
>
> import Control.Lens (makeLenses, (^.), (%~), (&))
>
> import qualified Data.Map as M
> import Data.Maybe (fromMaybe)
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

> data Edge =
>   Edge 
>      { _from :: Int
>      , _to  :: Int
>      }
>   deriving (Show)
> makeLenses ''Edge

> data DUG n =
>   DUG 
>      { _operations :: [Node n]
>      ,  _arguments :: M.Map Int [Edge]
>      }
>   deriving (Show)
> makeLenses ''DUG

Initialisation and creation:

> emptyDug :: DUG n
> emptyDug = DUG [] M.empty
>
> insertEdge :: Int -> Int -> DUG n -> DUG n
> insertEdge i j (d @ (DUG o a)) = d & arguments %~ (M.insert i edges')
>   where
>       edges = a M.! i
>       edge = Edge i j
>       edges' = edges ++ [edge]
>
> insertOp :: Node n -> DUG n -> DUG n
> insertOp node d = d & operations %~ (++ [node])
>                     & arguments %~ addEmptyEdges node
>                     & arguments %~ updateEdges node (node ^. nodeArgs)

> addEmptyEdges :: Node n -> M.Map Int [Edge] -> M.Map Int [Edge]
> addEmptyEdges n edges = M.insert (n ^. nodeIndex) nEdges' edges
>   where
>       nEdges = M.lookup (n ^. nodeIndex) edges
>       nEdges' = fromMaybe [] nEdges

> updateEdges :: Node n -> [DUGArg] -> M.Map Int [Edge] -> M.Map Int [Edge]
> updateEdges n [] edges = edges
> updateEdges n (d:ds) edges = updateEdges n ds (updateEdge n d edges)

> updateEdge :: Node n -> DUGArg -> M.Map Int [Edge] -> M.Map Int [Edge]
> updateEdge n (S.Version i) edges = M.insert i iEdges' edges
>   where
>       maybeEdges = M.lookup i edges
>       iEdges = fromMaybe [] maybeEdges 
>       iEdges' = edge : iEdges
>       edge = Edge i (n ^. nodeIndex)
> updateEdge _ _ edges = edges

> generateNode :: S.Operation -> [DUGArg] -> DUG n -> n -> Node n
> generateNode op args d n = Node op args ni n
>   where
>       ni = d ^. operations & length

There are many things one would like to extract from a DUG, which are easy with simple combinators:

> edges :: DUG n -> [Edge]
> edges d = concat $ M.elems (d ^. arguments)

> successors :: DUG n -> Int -> [Edge]
> successors d i = (d ^. arguments) M.! i

> predecessors :: DUG n -> Int -> [Edge]
> predecessors d i = concat $ map (filter (\e -> i == e ^. to)) (M.elems (d ^. arguments))

Debugging
---------

For debugging a pretty-printing function is defined:

> pprintDUG :: DUG n -> String 
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

> pprintEdge :: Edge -> String
> pprintEdge e = ""

> pprintDArg :: DUGArg -> String 
> pprintDArg (S.Version i) = "v" ++ show i
> pprintDArg (S.NonVersion i) = show i

and conversion to GraphViz:

> dug2dot :: DUG n -> String -> IO ()
> dug2dot d fName = do
>   print "[dot/...] making dot..."
>   writeFile dotName ""
>   write "digraph G {"
>   print "[dot/...] writing nodes..."
>   write . unlines $ [show (n ^. nodeIndex) ++ "[label=\"" ++ pprintNode n ++ "\"]"  | n <- d ^. operations]
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
