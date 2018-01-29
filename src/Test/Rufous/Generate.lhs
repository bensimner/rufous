> {-# LANGUAGE StandaloneDeriving, ExistentialQuantification, TemplateHaskell #-}
> module Test.Rufous.Generate where
>
> import Control.Lens (makeLenses, makePrisms, (^.), (&), (%~), (.~))
> import Test.QuickCheck as QC
> import Control.Exception
> import Data.List
> import Data.Maybe
> import Data.Dynamic
> import qualified Data.Map as M
>
> import qualified Test.Rufous.DUG as D
> import qualified Test.Rufous.Signature as S
> import qualified Test.Rufous.Profile as P
> import qualified Test.Rufous.Random as R
> import qualified Test.Rufous.Exceptions as E

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
>       , _shadow :: Maybe Dynamic  -- if observer node this is Nothing
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

> data GenState =
>   GenState 
>       { _dug :: GenDUG
>       , _buffer :: [BufferedOperation]
>       , _sig    :: S.Signature
>       , _profile :: P.Profile
>       }
>   deriving (Show)
> makeLenses ''GenState

For debugging, a pretty-printing GenState function:

> pprintGenState :: GenState -> String
> pprintGenState st = pprinted
>   where
>       pprinted = unlines ["DUG:", dugRepr, "BUFFER:", bufferRepr]
>       dugRepr = lined $ lines (D.pprintDUG pprintBufNode show (st ^. dug))
>       bufferRepr = lined $ map pprintBufOp (st ^. buffer)
>       lined = unlines . map (" |" ++)
> 
> pprintBufNode :: BufferedNode -> String
> pprintBufNode n = node ++ "=" ++ name ++ " " ++ args
>   where
>       node = "v" ++ (n ^. nodeIndex & show)
>       name = n ^. nodeOperation ^. S.opName
>       args = intercalate " " (map pprintDArg (n ^. nodeArgs))
>
>
> pprintBufOp :: BufferedOperation -> String 
> pprintBufOp bop = (bop ^. bufOp ^. S.opName) ++ ": " ++ (show (bop ^. bufArgs))

> pprintBop :: BufferedOperation -> [BufferedArg] -> String 
> pprintBop bop args = (bop ^. bufOp ^. S.opName) ++ " " ++ intercalate " " (map pprintBufArg args)

> pprintBufArg :: BufferedArg -> String 
> pprintBufArg (Abstract _) = "x"
> pprintBufArg (Filled darg) = pprintDArg darg

> pprintDArg :: DUGArg -> String 
> pprintDArg (S.Version i) = "v" ++ show i
> pprintDArg (S.NonVersion i) = show i


Pipeline
========

Now the pipeline has multiple stages, starting from the empty DUG and empty state:

> emptyState :: S.Signature -> P.Profile -> GenState
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

> chooseOperation :: GenState -> IO BufferedOperation
> chooseOperation st = do
>   let p = st ^. profile
>   let ops = M.elems $ st ^. sig ^. S.operations
>   let weighted = map (\o -> (o, (p ^. P.operationWeights) M.! (o ^. S.opName))) ops
>   op <- R.chooseWeighted weighted
>   bop <- mkBufOp op
>   return bop

> inflate :: GenState -> IO GenState
> inflate st = inflateBuffer 10 st
>   where
>       inflateBuffer 0 st = return st
>       inflateBuffer n st = do
>           o <- chooseOperation st
>           let st' = st
>                   & buffer %~ (o :)
>           inflateBuffer (n - 1) st'

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
> deflate_boparg_nonversion :: BufferedOperation -> [BufferedNode] -> S.ArgType -> GenState -> IO (Maybe (S.Arg a Int))
> deflate_boparg_nonversion bop nodes atype st = do
>   n <- QC.generate (QC.arbitrary)
>   return $ Just (S.NonVersion n)
> 
> -- TODO: 
> deflate_boparg_version :: BufferedOperation -> [BufferedNode] -> S.ArgType -> GenState -> IO (Maybe (S.Arg Int a))
> deflate_boparg_version bop nodes atype st =
>   if not (null nodes) then do
>       n <- QC.generate (QC.elements nodes)
>       return $ Just (S.Version (n ^. nodeIndex))
>   else 
>       return Nothing
>

The GenDUG is built up alongside its shadow
but the shadow might fail to build -- the function may have a pre-condition

> runNode :: S.Implementation -> String -> [Dynamic] -> (Dynamic, S.ImplType)
> runNode impl opName dynArgs = (dynResult dynFunc dynArgs, t)
>   where
>       (dynFunc, t) = (impl ^. S.implOperations) M.! opName
>       dynResult f [] = f
>       dynResult f (a:as) = dynResult (f `dynApp` a) as

> data RunResult = RunSuccess Dynamic | RunTypeFail | RunExcept E.RufousException
> runDynamic :: S.ImplType -> Dynamic -> IO RunResult
> runDynamic (S.ImplType t) d = run t fromDynamic
>   where
>       run :: Typeable a => a -> (Dynamic -> Maybe a) -> IO RunResult
>       run _ f = do
>           case f d of
>               Nothing -> return $ RunTypeFail
>               Just r  -> catch (r `seq` return (RunSuccess d)) handleE
>       handleE :: E.RufousException -> IO RunResult
>       handleE e = return $ RunExcept e

> data ShadowResult = NoShadow | WasObserver | HasShadow Dynamic | ShadowGuardFailed
>   deriving (Show)
> tryMakeShadow :: GenDUG -> Maybe S.ShadowImplementation -> S.Operation -> [DUGArg] -> IO ShadowResult
> tryMakeShadow dug impl op args = 
>       case impl of
>           Just shadowImpl -> do
>               result <- runDynamic t dyn
>               case result of
>                   RunSuccess d -> return $ HasShadow d
>                   RunTypeFail  -> error "type mismatch."
>                   RunExcept e  ->
>                       case e of
>                           E.GuardFailed    -> return ShadowGuardFailed
>                           E.NotImplemented -> return $ HasShadow dyn
>           Nothing -> return NoShadow
>   where
>       dynArgs = map mkDyn args
>       mkDyn (S.Version i) = fromJust $ ((dug ^. D.operations) !! i) ^. shadow
>       mkDyn (S.NonVersion i) = toDyn $ i
>       -- the fromJust here is safe -- it only gets evaluated in the `just` branch above
>       (dyn, t) = runNode (fromJust impl) (op ^. S.opName) dynArgs
>
> -- update the state after
> deflate_commitOperation :: BufferedOperation -> [BufferedArg] -> GenState -> IO (Maybe GenState)
> deflate_commitOperation bop bargs st = do
>   let args = map unFilled bargs
>   print ("tryCommit..", pprintBop bop bargs)
>   print ("state:", st ^. dug ^. D.operations & map pprintBufNode)
>   shadow <- tryMakeShadow (st ^. dug) (st ^. sig ^. S.shadowImpl) (bop ^. bufOp) args
>   print ("commit", shadow)
>   case shadow of
>       NoShadow -> mkNewNode args Nothing
>       WasObserver -> mkNewNode args Nothing
>       HasShadow s -> mkNewNode args $ Just s
>       _ -> return Nothing
>   where
>       mkNewNode args s = do
>           let i = st ^. dug ^. D.operations & length
>           let node = BufferedNode (bop ^. bufOp) args i s
>           return $ Just $
>               st 
>               & dug %~ (D.insertOp node)
>               & dug %~ (insertEdges i (node ^. nodeArgs))
>       insertEdges :: Int -> [DUGArg] -> GenDUG -> GenDUG
>       insertEdges i (S.Version v : args) d = D.insertEdge v i BufferedEdge (insertEdges i args d)
>       insertEdges i (S.NonVersion v : args) d = insertEdges i args d
>       insertEdges i [] d = d
>           
> -- try fix some arguments for a buffered operation from the state
> -- and return the new state
> deflate_bopargs :: BufferedOperation -> GenState -> IO GenState
> deflate_bopargs bop st = do
>       -- try fill the args up with valid arguments from the DUG
>       bargs' <- go ((bop ^. bufArgs) `zip` (validArgs bop st)) []
>       -- try commit args that satisfy potential guard
>       if all isFilled bargs' then do
>           committed <- deflate_commitOperation bop bargs' st
>           case committed of
>               Nothing -> return $ st & buffer %~ (bop:)  -- todo: better re-try semantics
>               Just st' -> return st'
>       else
>           return$ 
>               st 
>               & buffer %~ (bop:)  -- return the argument to the buffer and continue
>   where
>       --go :: [(BufferedArg, [BufferedNode])] -> [BufferedArg] -> IO [BufferedArg]
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
> deflate_bops :: [BufferedOperation] -> GenState -> IO GenState
> deflate_bops [] st = return st
> deflate_bops (bop:bops) st = do
>   -- take the head/tail of the buffer
>   st' <- deflate_bopargs bop st
>   deflate_bops bops st'
> 
> deflate :: GenState -> IO GenState
> deflate st = do
>   let bops = st ^. buffer
>   let st'  = st & buffer .~ []
>   deflate_bops bops st'

Model and Pre-condition checking
--------------------------------

The first thing to check is that there are enough nodes in the DUG to use as version arguments
This happens before any arguments are ever chosen

> -- choose a set of valid arguments for each Node
> validArgs :: BufferedOperation -> GenState -> [[BufferedNode]]
> validArgs bop st = do
>   v <- bop ^. bufArgs
>   case v of
>       Filled _                  -> return []
>       Abstract (S.NonVersion _) -> return []
>       Abstract (S.Version    _) -> return $ do
>           node <- st ^. dug ^. D.operations
>           return node
