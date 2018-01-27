> {-# LANGUAGE TemplateHaskell #-}
> module Test.Rufous.Generate where
>
> import Control.Lens (makeLenses, makePrisms, (^.), (&), (%~), (.~))
> import Test.QuickCheck as QC
>
> import Data.Maybe
> import qualified Data.Map as M
>
> import qualified Test.Rufous.DUG as D
> import qualified Test.Rufous.Signature as S
> import qualified Test.Rufous.Profile as P
> import qualified Test.Rufous.Random as R

Generating a DUG is a multi-step process.
The core algorithm attempts to build a DUG step by step,
keeping a fully-constructed DUG and adding to it in a loop.

Algorithm:
    - Start with the empty DUG and an empty buffer
    - At each step choose a new operation, and append it to the operations buffer

During Generation a lot of state needs to be kept:
    - Firstly the DUG itself has special Nodes that store information about the operation that created it:

> type DUGArg = S.Arg Int Int
> data BufferedArg = Abstract S.ArgType | Filled DUGArg
>   deriving (Show)
> makePrisms ''BufferedArg
> 
> unFilled :: BufferedArg -> DUGArg
> unFilled (Filled a) = a
> unFilled _          = error "unFilled :: Recieved Abstract"
>
> isFilled :: BufferedArg -> Bool
> isFilled (Filled a) = True
> isFilled _          = False
>
> data BufferedNode =
>   BufferedNode
>       { _nodeOperation :: S.Operation
>       , _nodeArgs      :: [DUGArg]
>       , _nodeIndex     :: Int
>       } 
>   deriving (Show)
> makeLenses ''BufferedNode
>
> data BufferedEdge = BufferedEdge
>   deriving (Show)

    - Not only that, but Rufous must also store a buffer of operations to perform

> data BufferedOperation =
>   BufferedOperation 
>       { _bufOp     :: S.Operation
>       , _bufArgs   :: [BufferedArg]
>       , _persistent :: Bool
>       }
>   deriving (Show)
> makeLenses ''BufferedOperation

The state is just the product of these types with information about the ADT:

> type GenDUG = D.DUG BufferedNode BufferedEdge

> data GenState shadow =
>   GenState 
>       { _dug :: GenDUG
>       , _buffer :: [BufferedOperation]
>       , _sig    :: S.Signature shadow
>       , _profile :: P.Profile
>       }
>   deriving (Show)
> makeLenses ''GenState

For debugging, a pretty-printing GenState function:

> pprintGenState :: GenState shadow -> String
> pprintGenState st = pprinted
>   where
>       pprinted = unlines ["DUG:", dugRepr, "BUFFER:", bufferRepr]
>       dugRepr = lined $ lines (D.pprintDUG show show (st ^. dug))
>       bufferRepr = lined $ map pprintBufOp (st ^. buffer)
>       lined = unlines . map (" |" ++)
>
> pprintBufOp :: BufferedOperation -> String 
> pprintBufOp bop = (bop ^. bufOp ^. S.opName) ++ ": " ++ (show (bop ^. bufArgs))


Pipeline
========

Now the pipeline has multiple stages, starting from the empty DUG and empty state:

> emptyState :: S.Signature shadow -> P.Profile -> GenState shadow
> emptyState s p = GenState { _dug=D.emptyDug, _buffer=[], _sig=s, _profile=p }

Stage 1
-------

Stage 1 is the inflation stage

The first step is to pick a new operation to add to the DUG:
    - It must follow the profile's weights for each operation as much as possible
    - Disclude operations disallowed by pre-conditions
Then choose properties of the operation:
    - Whether this application is persistent
Then add this operation to the front of the buffer

> mkBufOp :: S.Operation -> IO BufferedOperation
> mkBufOp op = do
>   persistent <- R.randomBool 0.1
>   let args = op ^. S.opSig ^. S.opArgs 
>   let abstractArgs = map Abstract args
>   let bop = BufferedOperation op abstractArgs persistent
>   return $ bop

> chooseOperation :: GenState shadow -> IO BufferedOperation
> chooseOperation st = do
>   let p = st ^. profile
>   let ops = M.elems $ st ^. sig ^. S.operations
>   let weighted = map (\o -> (o, (p ^. P.operationWeights) M.! (o ^. S.opName))) ops
>   op <- R.chooseWeighted weighted
>   bop <- mkBufOp op
>   return bop

> inflate :: GenState shadow -> IO (GenState shadow)
> inflate st = do
>   -- choose an operation
>   o <- chooseOperation st
>   return $ st
>          & buffer %~ (o :)

Stage 2 
-------

Now to deflate the buffer, and commit the operations to the DUG.

This is done by traversing the buffer and applying the following operation to each buffered operation:
    - Look at head of the remaining types
    - If a non-version argument, 
        - pick one at random
    - Else if a version argument
        - If cannot find a node in DUG that "fits" then
            - Return this operation back to the buffer
        - Otherwise
            - commit that node to that argument

> -- choose a Non-Version argument from the state for some buffered operation
> deflate_boparg_nonversion :: BufferedOperation -> [BufferedNode] -> S.ArgType -> GenState shadow -> IO (Maybe (S.Arg a Int))
> deflate_boparg_nonversion bop nodes atype st = do
>   n <- QC.generate (QC.arbitrary)
>   return $ Just (S.NonVersion n)
> 
> -- TODO: 
> deflate_boparg_version :: BufferedOperation -> [BufferedNode] -> S.ArgType -> GenState shadow -> IO (Maybe (S.Arg Int a))
> deflate_boparg_version bop nodes atype st =
>   if not (null nodes) then do
>       n <- QC.generate (QC.elements nodes)
>       return $ Just (S.Version (n ^. nodeIndex))
>   else 
>       return Nothing
>
> -- update the state after
> deflate_commitOperation :: BufferedOperation -> [BufferedArg] -> GenState shadow -> IO (GenState shadow)
> deflate_commitOperation bop bargs st = do
>   let i = st ^. dug ^. D.operations & length
>   let node = BufferedNode (bop ^. bufOp) (map unFilled bargs) i
>   return $
>       st 
>       & dug %~ (D.insertOp node)
>       & dug %~ (insertEdges i (node ^. nodeArgs))
>   where
>       insertEdges :: Int -> [DUGArg] -> D.DUG BufferedNode BufferedEdge -> D.DUG BufferedNode BufferedEdge
>       insertEdges i (S.Version v : args) d = D.insertEdge i v BufferedEdge (insertEdges i args d)
>       insertEdges i (S.NonVersion v : args) d = insertEdges i args d
>       insertEdges i [] d = d
>           
> -- try fix some arguments for a buffered operation from the state
> -- and return the new state
> deflate_bopargs :: BufferedOperation -> GenState shadow -> IO (GenState shadow)
> deflate_bopargs bop st = do
>       -- try fill the args up with valid arguments from the DUG
>       bargs' <- go ((bop ^. bufArgs) `zip` (validArgs bop st)) []
>       -- try commit args that satisfy potential guard
>       if all isFilled bargs' && checkValid bop bargs' st then
>           deflate_commitOperation bop bargs' st
>       else
>           return$ 
>               st 
>               & buffer %~ (bop:)  -- return the argument to the buffer and continue
>   where
>       go :: [(BufferedArg, [BufferedNode])] -> [BufferedArg] -> IO [BufferedArg]
>       go [] collected = return $ collected
>       go ((ba, bnodes) : bargs) collected = do
>           arg <- case ba of 
>               Filled   x     -> return $ Nothing  -- Nothing => No Change
>               Abstract atype -> case atype of
>                   S.Version    _ -> deflate_boparg_version bop bnodes atype st
>                   S.NonVersion _ -> deflate_boparg_nonversion bop bnodes atype st
>           case arg of 
>               Nothing     -> go bargs (collected ++ [ba])
>               Just update -> case update of 
>                   S.NonVersion x -> go bargs (collected ++ [Filled update])
>                   S.Version    x -> go bargs (collected ++ [Filled update])
>           
> deflate_bops :: [BufferedOperation] -> GenState shadow -> IO (GenState shadow)
> deflate_bops [] st = return st
> deflate_bops (bop:bops) st = do
>   -- take the head/tail of the buffer
>   st' <- deflate_bopargs bop st
>   deflate_bops bops st'
> 
> deflate :: GenState shadow -> IO (GenState shadow)
> deflate st = do
>   let bops = st ^. buffer
>   let st'  = st & buffer .~ []
>   deflate_bops bops st'

Model and Pre-condition checking
--------------------------------

The first thing to check is that there are enough nodes in the DUG to use as version arguments
This happens before any arguments are ever chosen

> -- choose a set of valid arguments for each Node
> validArgs :: BufferedOperation -> GenState shadow -> [[BufferedNode]]
> validArgs bop st = do
>   v <- bop ^. bufArgs
>   case v of
>       Filled _                  -> return []
>       Abstract (S.NonVersion _) -> return []
>       Abstract (S.Version    _) -> return $ do
>           node <- st ^. dug ^. D.operations
>           return node

> checkValid :: BufferedOperation -> [BufferedArg] -> GenState shadow -> Bool
> checkValid _ _ _ = True
