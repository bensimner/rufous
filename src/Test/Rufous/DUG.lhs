> {-# LANGUAGE TemplateHaskell #-}
> module Test.Rufous.DUG where
>
> import qualified Prelude as P
> import Prelude hiding (head, tail)
> 
> import Lens.Micro ((^.), (^..), (%~), (&), _1, _2, (.~), at, _Just)
> import Lens.Micro.TH (makeLenses)
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

The DUG is implemented as a reversed association array
where nodes store their predecessors -- the arguments of the operation

> data DUG n =
>   DUG 
>      { _operations :: M.Map Int (Node n, [Int])
>      , _dugName :: Maybe String
>      }
>   deriving (Show)
> makeLenses ''DUG

Initialisation and creation:

> emptyDug :: DUG n
> emptyDug = DUG M.empty Nothing

> insertArgument :: Int -> Int -> DUG n -> DUG n
> insertArgument version arg d = d & operations %~ M.update (\n -> Just $ n & _2 %~ (++ [arg])) version
>
> insertOp :: Node n -> DUG n -> DUG n
> insertOp node d = d & operations %~ (M.insert (node ^. nodeIndex) (node, []))
>                     & operations %~ updateEdges node (node ^. nodeArgs)

> updateEdges :: Node n -> [DUGArg] -> M.Map Int (Node n, [Int]) -> M.Map Int (Node n, [Int])
> updateEdges n [] edges = edges
> updateEdges n (d:ds) edges = updateEdges n ds (updateEdge n d edges)

> updateEdge :: Node n -> DUGArg -> M.Map Int (Node n, [Int]) -> M.Map Int (Node n, [Int])
> updateEdge n (S.Version i) = M.update f (n ^. nodeIndex)
>   where
>       f (n, xs) = Just (n, xs ++ [i])
> updateEdge _ _ = id

> generateNode :: S.Operation -> [DUGArg] -> DUG n -> n -> Node n
> generateNode op args d n = Node op args ni n
>   where
>       ni = d ^. operations & length

There are many things one would like to extract from a DUG, which are easy with simple combinators:

> data ArgEdge = E { to :: Int, from :: Int }
> edges :: DUG n -> [ArgEdge]
> edges d = [( E (n ^. nodeIndex) x) | (n, xs) <- d ^. operations & M.elems, x <- xs]

> nodes :: DUG n -> [Node n]
> nodes d = d ^.. operations . traverse . _1

> opName :: DUG n -> Int -> String
> opName d i = ((d ^. operations) M.! i) ^. _1 ^. nodeOperation ^. S.opName

> opType :: DUG n -> Int -> S.OperationType
> opType d i = ((d ^. operations) M.! i) ^. _1 ^. nodeOperation ^. S.opSig ^. S.opType

> nodeValue :: DUG n -> Int -> n
> nodeValue d i = ((d ^. operations) M.! i) ^. _1 . node

Debugging
---------

For debugging a pretty-printing function is defined:

> pprintDUG :: DUG n -> String 
> pprintDUG = flip pprintDUG' pprintNode

> pprintDUG' :: DUG n -> (Node n -> String) -> String 
> pprintDUG' d f = lined ["Operations:", operationsRepr, "Arguments:", argumentsRepr]
>   where
>       operationsRepr = lined [f n | (n, _) <- d ^. operations & M.elems]
>       argumentsRepr  = lined ["n" ++ show n ++ " -> " ++ show es | (n, (_, es)) <- d ^. operations & M.toList]
>       lined = unlines . map (" + " ++)

> pprintNode :: Node n -> String
> pprintNode n = node ++ "=" ++ name ++ " " ++ args
>   where
>       node = "v" ++ (n ^. nodeIndex & show)
>       name = n ^. nodeOperation ^. S.opName
>       args = intercalate " " (map pprintDArg (n ^. nodeArgs))

> pprintDArg :: DUGArg -> String 
> pprintDArg (S.Version i) = "v" ++ show i
> pprintDArg (S.NonVersion (S.VersionParam i)) = show i
> pprintDArg (S.NonVersion (S.IntArg i)) = show i
> pprintDArg (S.NonVersion (S.BoolArg i)) = show i

and conversion to GraphViz:

> dug2dot :: DUG n -> String -> IO ()
> dug2dot d s = dug2dot' d pprintNode s

> dug2dot' :: DUG n -> (Node n -> String) -> String -> IO ()
> dug2dot' d fN fName = do
>   putStrLn $ "[dot/...] writing dug " ++ fName ++ " ..."
>   writeFile dotName ""
>   write "digraph G {"
>   write "overlap=\"false\""
>   write . unlines $ [show (n ^. nodeIndex) ++ "[label=\"" ++ fN n ++ "\"]"  | n <- d ^. operations ^.. traverse . _1]
>   write . unlines $ [show from ++ "->" ++ show to  | E to from <- edges d]
>   write "}"
>   createProcess (proc "neato" [dotName, "-Tpng", "-o", pngName])
>   return ()
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

> isVersion :: Node n -> Bool
> isVersion n = n ^. nodeOperation ^. S.opSig ^. S.opType /= S.Observer

> safeDiv a b = if b == 0 then 0 else a / b

> extractProfile :: S.Signature -> DUG n -> P.Profile
> extractProfile s d = P.Profile weights pWeights mortality
>   where
>       argKeys = St.fromList $ map from $ filter ((/= S.Observer) . opType d . to) $ edges d
>       totalNodes = fromIntegral $ M.size $ d ^. operations 
>       totalVersions = fromIntegral $ M.size $ M.filter (isVersion . fst) $ d ^. operations 
>       livingVersions = fromIntegral $ St.size argKeys
>       mortality = 1 - (livingVersions `safeDiv` totalVersions)
>       weights = (`safeDiv` totalNodes) <$> countOperations s d
>       pWeights = persistentWeights s d

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
> countOperations s d = go (d ^. operations ^.. traverse . _1) (emptyMap s)
>   where
>       go [] m = m
>       go (o:os) m = let m' = update (o ^. nodeOperation ^. S.opName) (+1) 1 m
>                     in go os m'

> countArguments :: S.Signature -> DUG n -> M.Map String Float
> countArguments s d = go (edges d) (emptyMap s)
>   where
>       go [] m = m
>       go ((E i j):is) m = let m' = M.insertWith (+) (opName d i) 1 m
>                           in go is m'

> sameKind :: DUG n -> Int -> Int -> Bool
> sameKind d i j =
>   case (opType d i, opType d j) of
>       (S.Observer, S.Observer) -> True
>       (S.Observer, _) -> False
>       (_, S.Observer) -> False
>       (_, _) -> True

Computing the persistent weights is harder:

> persistentWeights :: S.Signature -> DUG n -> M.Map String Float
> persistentWeights s d = M.intersectionWith (safeDiv) persists args
>   where edgers = map f (edges d)
>         f (E to from) = (from, opType d to, opName d to)
>         dups [] s = []
>         dups ((f,t,v):xs) s = let ds = dups xs (St.insert (f, t) s) 
>                               in if (f, t) `St.member` s then v:ds else ds
>         count [] c = c
>         count (x:xs) c = M.insertWith (+) x 1 (count xs c)
>         persists = count (dups edgers St.empty) (emptyMap s)
>         args = countArguments s d

> instance Functor DUG where
>   fmap f d = d & operations %~ (fmap (_1 %~ fmap f))

> instance Functor Node where
>   fmap f n = n & node %~ f

> foldDug :: (b -> Node a -> b) -> b -> DUG a -> b
> foldDug f v d = foldl f v ((d ^. operations & M.elems) ^.. traverse . _1)

> mapDug :: (Node a -> Node b) -> DUG a -> DUG b
> mapDug f d = d & operations %~ M.map (\n -> n & _1 %~ f)

> instance Applicative Node where
>   pure x = Node undefined undefined undefined x
>   fn <*> x = x & node %~ (fn ^. node)

> instance Applicative DUG where
>   pure x = DUG (M.fromList [(0, (pure x, []))]) Nothing
>   fdug <*> d = newDug
>       where
>           nodes = zip (M.toList (fdug ^. operations)) (M.toList (d ^. operations))
>           newNodes = map (\((i, (f, _)), (_, (f', xs))) -> (i, (f <*> f', xs))) nodes
>           newDug = d & operations .~ M.fromList newNodes

> updateNodes :: [Node b] -> DUG a -> DUG b
> updateNodes ns d = d & operations %~ f
>   where f m = M.fromList $ map g $ zip ns (M.toList m)
>         g (n, (i, (k, xs))) = (i, (k & node .~ n ^. node, xs))

> sequenceDugIO :: DUG (IO a) -> IO (DUG a)
> sequenceDugIO d = do 
>   nodeValues <- sequence $ map (^. node) (nodes d)
>   let zipped = zip (nodes d) nodeValues
>   let new = map (\(n, v) -> n & node .~ v) zipped
>   return $ updateNodes new d
