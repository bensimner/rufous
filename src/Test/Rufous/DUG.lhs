> {-# LANGUAGE TemplateHaskell #-}
> module Test.Rufous.DUG where
> 
> import Control.Lens (makeLenses, (^.), (%~), (&))
>
> import qualified Data.Map as M
> import qualified Data.Set as St
> import Data.Maybe (fromMaybe)
> import System.Process
> import Data.List (intercalate)

> import Test.Rufous.Random
> import qualified Test.Rufous.Signature as S
> import qualified Test.Rufous.Profile as P

The core type that Rufous deals with is the Datatype-Usage-Graph (DUG).
Control flow through rufous is typically transformations of and between DUG types.

Fundamentally a DUG is simply a directed-acyclic-graph where nodes represent results of applications
and edges are the arguments.

The usual node/edge types are simple:

> type DUGArg = S.Arg Int Int Int Bool 
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
>      , _dugName :: Maybe String
>      }
>   deriving (Show)
> makeLenses ''DUG

Initialisation and creation:

> emptyDug :: DUG n
> emptyDug = DUG [] M.empty Nothing
>
> insertEdge :: Int -> Int -> DUG n -> DUG n
> insertEdge i j d = d & arguments %~ (M.insert i edges')
>   where
>       edges = (d ^. arguments) M.! i
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

> successors :: DUG n -> Int -> [Int]
> successors d i = [e ^. to | e <- (d ^. arguments) M.! i]

> predecessors :: DUG n -> Int -> [Int]
> predecessors d i = [e ^. to | e <- concat . map (filter (\e -> i == e ^. to)) $ (M.elems (d ^. arguments))]

> opName :: DUG n -> Int -> String
> opName d i = ((d ^. operations) !! i) ^. nodeOperation ^. S.opName

> opType :: DUG n -> Int -> S.OperationType
> opType d i = ((d ^. operations) !! i) ^. nodeOperation ^. S.opSig ^. S.opType

Debugging
---------

For debugging a pretty-printing function is defined:

> pprintDUG :: DUG n -> String 
> pprintDUG = flip pprintDUG' pprintNode

> pprintDUG' :: DUG n -> (Node n -> String) -> String 
> pprintDUG' d f = lined ["Operations:", operationsRepr, "Arguments:", argumentsRepr]
>   where
>       operationsRepr = lined [f n | n <- d ^. operations]
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
> pprintDArg (S.NonVersion (S.VersionParam i)) = show i
> pprintDArg (S.NonVersion (S.IntArg i)) = show i
> pprintDArg (S.NonVersion (S.BoolArg i)) = show i

and conversion to GraphViz:

> dug2dot :: DUG n -> String -> IO ()
> dug2dot d s = dug2dot' d pprintNode pprintEdge s

> dug2dot' :: DUG n -> (Node n -> String) -> (Edge -> String) -> String -> IO ()
> dug2dot' d fN fE fName = do
>   print "[dot/...] making dot..."
>   writeFile dotName ""
>   write "digraph G {"
>   print "[dot/...] writing nodes..."
>   write . unlines $ [show (n ^. nodeIndex) ++ "[label=\"" ++ fN n ++ "\"]"  | n <- d ^. operations]
>   print "[dot/...] writing edges..."
>   write . unlines $ [show (e ^. from) ++ "->" ++ show (e ^. to) ++ "[label=\"" ++ fE e ++ "\"]"  | e <- edges d]
>   write "}"
>   print "[dot/...] written dot, compiling to png..."
>   createProcess (proc "dot" [dotName, "-Tpng", "-o", pngName])
>   print "[dot/done]"
>   where
>       write s = appendFile dotName (s ++ "\n")
>       dotName = fName ++ ".dot"
>       pngName = fName ++ ".png"

Profile Extraction
------------------

Extracting a profile from a DUG is fairly straightforward:
    - To extract mortality see how many nodes are source in arguments
    - To extract weights of operations see how many nodes for each operation
    - To extract persistent applications of each operation is harder:
        - For each *edge*, count up how many originate from
          a node with another outgoing edge to another 
          operation with the same kind

> extractProfile :: S.Signature -> DUG n -> P.Profile
> extractProfile s d = P.Profile weights persistentApplicationWeights mortality
>   where
>       argKeys = d ^. arguments & M.keysSet
>       totalNodes = fromIntegral $ length $ d ^. operations 
>       livingNodes = fromIntegral $ length $ filter (\n -> (n ^. nodeIndex) `St.member` argKeys) (d ^. operations)
>       mortality = 1 - (livingNodes / totalNodes)
>       safeDiv a b = if b == 0 then 0 else a / b
>       weights = (`safeDiv` totalNodes) <$> countOperations s d
>       persistentOpCount = persistentWeights s d
>       argCounts = countArguments s d
>       persistentApplicationWeights = M.mapWithKey (\k a -> a `safeDiv` (argCounts M.! k)) persistentOpCount

> update :: Ord k => k -> (a -> a) -> a -> M.Map k a -> M.Map k a
> update k f v m = M.insert k v' m
>   where
>       v' = fromMaybe v (f <$> (M.lookup k m))

> emptyMap :: Num a => S.Signature -> M.Map String a
> emptyMap s = go (s ^. S.operations & M.keys) M.empty
>   where
>       go [] m = m
>       go (s:ss) m = M.insert s 0 $ go ss m

> countOperations :: Num a => S.Signature -> DUG n -> M.Map String a
> countOperations s d = go (d ^. operations) (emptyMap s)
>   where
>       go [] m = m
>       go (o:os) m = let m' = update (o ^. nodeOperation ^. S.opName) (+1) 1 m
>                     in go os m'

> countArguments :: Num a => S.Signature -> DUG n -> M.Map String a
> countArguments s d = go (d ^. arguments & M.keys) (emptyMap s)
>   where
>       go [] m = m
>       go (i:is) m = let m' = update (opName d i) (+1) 1 m
>                     in go is m'

> sameKind :: DUG n -> Int -> Int -> Bool
> sameKind d i j =
>   case (opType d i, opType d j) of
>       (S.Observer, S.Observer) -> True
>       (S.Observer, _) -> False
>       (_, S.Observer) -> False
>       (_, _) -> True

> persistentWeights :: Num a => S.Signature -> DUG n -> M.Map String a
> persistentWeights s d = go (concat (d ^. arguments & M.elems)) (emptyMap s)
>   where
>       go [] m = m
>       go (e:es) m =
>           let name = opName d (e ^. to) in
>           let succs = successors d (e ^. from) in
>           let m' = if 2 <= (length $ filter (sameKind d (e ^. to)) succs)
>               then update name (+1) 1 m
>               else update name (+0) 0 m
>           in go es m'

> instance Functor DUG where
>   fmap f d = d & operations %~ map (fmap f)

> instance Functor Node where
>   fmap f n = n & node %~ f

> foldDug :: (b -> Node a -> b) -> b -> DUG a -> b
> foldDug f v d = foldl f v (d ^. operations)
