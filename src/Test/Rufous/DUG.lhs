> {-# LANGUAGE TemplateHaskell #-}
> module Test.Rufous.DUG where
>
> import Control.Lens (makeLenses, (^.), (%~), (&))
>
> import qualified Data.Map as M
> import Data.Maybe
> 
> import Test.Rufous.Random

The core type that Rufous deals with is the Datatype-Usage-Graph (DUG).
Control flow through rufous is typically transformations of and between DUG types.

Fundamentally a DUG is simply a directed-acyclic-graph where nodes represent results of applications
and edges are the arguments.

> data Edge e =
>   Edge 
>      { _from :: Int
>      , _to  :: Int
>      , _edge :: e
>      }
>   deriving (Show)
> makeLenses ''Edge
>
> data DUG n e =
>   DUG 
>      { _operations :: [n]
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
> insertOp :: n -> DUG n e -> DUG n e
> insertOp n d = d & operations %~ (++ [n])

There are many things one would like to extract from a DUG, which are easy with simple combinators:

> successors :: DUG n e -> Int -> [Edge e]
> successors d i = (d ^. arguments) M.! i
> 
> predecessors :: DUG n e -> Int -> [Edge e]
> predecessors d i = concat $ map (filter (\e -> i == e ^. to)) (M.elems (d ^. arguments))

For debugging a pretty-printing function is defined:

> pprintDUG :: (n -> String) -> (e -> String) -> DUG n e -> String 
> pprintDUG nf ef d = lined ["Operations:", operationsRepr, "Arguments:", argumentsRepr]
>   where
>       operationsRepr = lined ["n" ++ nf n | n <- d ^. operations]
>       argumentsRepr  = lined ["n" ++ show n ++ " -> " ++ show (map edgesRepr es) | (n, es) <- d ^. arguments & M.toList]
>       edgesRepr e = show (e ^. to) ++ "(" ++ ef (e ^. edge) ++ ")"
>       lined = unlines . map (" + " ++)

