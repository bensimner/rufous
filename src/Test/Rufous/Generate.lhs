> {-# LANGUAGE StandaloneDeriving, ExistentialQuantification, TemplateHaskell #-}
> module Test.Rufous.Generate where
>
> import Control.Lens (makeLenses, (^.), (&), (%~), (.~))
> import Test.QuickCheck as QC
> import Control.Exception
> import Data.Maybe (fromJust, fromMaybe)
> import Data.List (intercalate)
> import Data.Dynamic
> import System.IO.Unsafe

> import qualified Data.Map as M
> import qualified Data.Set as St
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
    - Not only that, but Rufous must also store a buffer of operations to perform

> data BufferedArg = Abstract S.ArgType | Filled D.DUGArg
>   deriving (Show)

> data BufferedOperation =
>   BufferedOperation 
>       { _bufOp     :: S.Operation
>       , _bufArgs   :: [BufferedArg]
>       , _persistent :: Bool
>       }
>   deriving (Show)
> makeLenses ''BufferedOperation

Each stored node has some additional state:
    - Whether the node is dead or alive.
        Dead nodes are not mutated further
    - Whether the node is an infant
        - Infant nodes have just been created and not been mutated yet.
    - Whether the node is ephemeral
        - Ephemeral nodes should only be used once
    - Whether the node is persistent
        - Persistent nodes cannot be used except by persistent applications

> data OperationNodes =
>   OperationNodes
>       { _infants :: St.Set Int
>       , _persistents :: St.Set Int
>       }
>   deriving (Show)       
> makeLenses ''OperationNodes

> data GenNodeState =
>   GenNodeState
>       { _shadow :: Maybe Dynamic
>       }
>   deriving (Show)
> makeLenses ''GenNodeState



> type BufferedNode = D.Node GenNodeState
> type GenDUG = D.DUG GenNodeState

The state is just the product of these types with information about the ADT:

> data GenState =
>   GenState 
>       { _dug :: GenDUG
>       , _buffer :: [BufferedOperation]
>       , _sig    :: S.Signature
>       , _profile :: P.Profile
>       , _livingNodes :: St.Set Int
>       , _mutatorNodes :: OperationNodes
>       , _observerNodes :: OperationNodes
>       }
>   deriving (Show)
> makeLenses ''GenState

For debugging, a pretty-printing GenState function:

> pprintGenState :: GenState -> String
> pprintGenState st = pprinted
>   where
>       pprinted = unlines ["DUG:", dugRepr, "BUFFER:", bufferRepr]
>       dugRepr = lined $ lines (D.pprintDUG (st ^. dug))
>       bufferRepr = lined $ map pprintBufOp (st ^. buffer)
>       lined = unlines . map (" |" ++)
> 
>
> pprintBufOp :: BufferedOperation -> String 
> pprintBufOp bop = (bop ^. bufOp ^. S.opName) ++ ": " ++ (show (bop ^. bufArgs))

> pprintBop :: BufferedOperation -> [BufferedArg] -> String 
> pprintBop bop args = (bop ^. bufOp ^. S.opName) ++ " " ++ intercalate " " (map pprintBufArg args)

> pprintBufArg :: BufferedArg -> String 
> pprintBufArg (Abstract _) = "x"
> pprintBufArg (Filled darg) = D.pprintDArg darg


Pipeline
========

Now the pipeline has multiple stages, starting from the empty DUG and empty state:

> emptyState :: S.Signature -> P.Profile -> GenState
> emptyState s p = GenState 
>   { _dug=D.emptyDug
>   , _buffer=[]
>   , _sig=s
>   , _profile=p
>   , _livingNodes=St.empty
>   , _mutatorNodes=OperationNodes St.empty St.empty
>   , _observerNodes=OperationNodes St.empty St.empty }

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
> inflate st = go 10 st
>   where
>       go 0 st = return st
>       go n st = do
>           st' <- inflateStep st
>           go (n-1) st'

> inflateStep :: GenState -> IO GenState
> inflateStep st = do
>   o <- chooseOperation st
>   let st' = st
>           & buffer %~ (o :)
>   return st'

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

To deflate the entire buffer, extract and and reset it before continuing to iterate deflate_bop:

> deflateAll :: GenState -> IO GenState
> deflateAll st = do
>   let bops = st ^. buffer
>   let st'  = st & buffer .~ []
>   deflate_bops bops st'

> deflate_bops :: [BufferedOperation] -> GenState -> IO GenState
> deflate_bops [] st = return st
> deflate_bops (bop:bops) st = do
>   st' <- deflate_bop bop st
>   deflate_bops bops st'

Deflating a single operation in a single step:

> deflateStep :: GenState -> IO GenState
> deflateStep st = do
>   let bop = st ^. buffer & head
>   let st' = st & buffer %~ tail
>   deflate_bop bop st'

To commit a single BufferedOperation is simple:
    - Try to collect a set of valid arguments
        - (if fail: return operation to buffer and continue)
    - Try to test guard/shadow
        - (if fail: return to buffer and continue)*
    - Commit new node to DUG and update old node states

*TODO:
This could be improved -- a failing pre-condition probably means that the non-version arguments
failed. Not that the entire operation is a fail.
Secondly, when contuning nothing stops this operation being attempted again with the same 
arguments in an infinite loop!

> deflate_bop :: BufferedOperation -> GenState -> IO GenState
> deflate_bop bop st = do
>       args' <- collectArgs bop st
>       if all isFilled args' then do
>           committed <- commitOperation bop args' st
>           case committed of
>               Nothing -> return $ st & buffer %~ (bop:)  -- todo: better re-try semantics
>               Just st' -> return st'
>       else
>           return$ 
>               st 
>               & buffer %~ (bop:)  -- return the argument to the buffer and continue

> collectArgs :: BufferedOperation -> GenState -> IO [BufferedArg]
> collectArgs bop st = do
>   let possibleArgs :: [(BufferedArg, [BufferedNode])]
>       possibleArgs = (bop ^. bufArgs) `zip` (validArgs bop st)
>   mapM (\(a, ns) -> collectArg bop a ns st) possibleArgs

> collectArg :: BufferedOperation -> BufferedArg -> [BufferedNode] -> GenState -> IO BufferedArg
> collectArg bop barg nodeChoices st = do
>   arg <- case barg of   
>       Filled x    -> return $ Nothing
>       Abstract at -> case at of
>           S.Version _    -> chooseVersionArg    bop nodeChoices at st
>           S.NonVersion a -> chooseNonVersionArg bop nodeChoices at st
>   case arg of
>       Nothing     -> return barg
>       Just update -> return (Filled update)

> chooseNonVersionArg :: BufferedOperation -> [BufferedNode] -> S.ArgType -> GenState -> IO (Maybe (S.Arg Int Int Int Bool))
> chooseNonVersionArg _ _ (S.NonVersion nva) _ = do
>   case nva of
>       S.IntArg       _ -> do
>           n <- QC.generate (QC.arbitrary)
>           return $ Just (S.NonVersion (S.IntArg n))
>       S.VersionParam _ -> do
>           n <- QC.generate (QC.arbitrary)
>           return $ Just (S.NonVersion (S.VersionParam n))
>       S.BoolArg _ -> do
>           b <- QC.generate (QC.arbitrary)
>           return $ Just (S.NonVersion (S.BoolArg b))
> 
> -- TODO: 
> chooseVersionArg :: BufferedOperation -> [BufferedNode] -> S.ArgType -> GenState -> IO (Maybe (S.Arg Int Int Int Bool))
> chooseVersionArg bop nodes atype st =
>   if not (null nodes) then do
>       n <- QC.generate (QC.elements nodes)
>       return $ Just (S.Version (n ^. D.nodeIndex))
>   else 
>       return Nothing
>   

The DUG is built up alongside its shadow
This is done by attempting to "commit" the operation to the DUG
and failing if the shadow fails to compute in a well-constrained way.

Specifically: if the shadow throws a ``Test.Rufous.Exceptions.GuardFailed`` exception,
then do not commit to the DUG and instead return Nothing.

When committed it's important to move the new node into the correct buckets:
    It is now a living node,
    An infant observer node,
    and an infant mutator node

> commitOperation :: BufferedOperation -> [BufferedArg] -> GenState -> IO (Maybe GenState)
> commitOperation bop bargs st = do
>   let args = map unFilled bargs
>   shadow <- tryMakeShadow (st ^. dug) (st ^. sig ^. S.shadowImpl) (bop ^. bufOp) args
>   alive <- R.randomBool (1 - (st ^. profile ^. P.mortality))
>   case shadow of
>       NoShadow -> mkNewNode args alive Nothing
>       WasObserver -> mkNewNode args alive Nothing
>       HasShadow s -> mkNewNode args alive $ Just s
>       _ -> return Nothing
>   where
>       mkNewNode args alive s = do
>           let nodeSt = GenNodeState s
>           let node = D.generateNode (bop ^. bufOp) args (st ^. dug) nodeSt
>           let i = node ^. D.nodeIndex
>           return $ Just $
>               st
>               & dug %~ (D.insertOp node)
>               & updateNodeLiving i alive
>               & updateNewNode i
>               & updateOldNodes args
>   
>       updateNewNode i st =
>           if bop ^. bufOp ^. S.opSig ^. S.opType /= S.Observer 
>               then
>                   st
>                   & mutatorNodes . infants %~ (St.insert i)
>                   & observerNodes . infants %~ (St.insert i)
>               else st
> 
>       updateNodeLiving i alive st =
>           if alive 
>               then st & livingNodes %~ (St.insert i)
>               else st
>
>       updateOldNodes [] st = st
>       updateOldNodes (arg:args) st = updateOldNodes args (updateNode arg st)
>       updateNode arg st = 
>           case (arg, bop ^. persistent, bop ^. bufOp ^. S.opSig ^. S.opType) of
>               (S.Version i, False, S.Observer) -> 
>                   st & observerNodes . infants %~ (St.delete i)
>               (S.Version i, False, _) -> 
>                   st & mutatorNodes . infants %~ (St.delete i)
>               (S.Version i, True, S.Observer) -> 
>                   st & observerNodes . infants %~ (St.delete i)
>                      & observerNodes . persistents %~ (St.insert i)
>               (S.Version i, True, _) -> 
>                   st & mutatorNodes . infants %~ (St.delete i)
>                      & mutatorNodes . persistents %~ (St.insert i)
>               _                      -> st
>           

> runNode :: S.Implementation -> String -> [Dynamic] -> (Dynamic, S.ImplType)
> runNode impl opName dynArgs = (dynResult dynFunc dynArgs, t)
>   where
>       (dynFunc, t) = (impl ^. S.implOperations) M.! opName
>       dynResult f [] = f
>       dynResult f (a:as) = dynResult (f `dynApp` a) as

> data RunResult = 
>   forall a. (Show a, Typeable a) => 
>   RunSuccess Dynamic a | RunTypeFail | RunExcept E.RufousException
> runDynamic :: S.ImplType -> Dynamic -> IO RunResult
> runDynamic (S.ImplType t) d = run t fromDynamic
>   where
>       run :: (Show a, Typeable a) => a -> (Dynamic -> Maybe a) -> IO RunResult
>       run _ f = do
>           case f d of
>               Nothing -> return RunTypeFail
>               Just r  -> catch (r `seq` return $ RunSuccess d r) handleE
>       handleE :: E.RufousException -> IO RunResult
>       handleE e = return $ RunExcept e

> data ShadowResult = NoShadow | WasObserver | HasShadow Dynamic | ShadowGuardFailed
>   deriving (Show)
> tryMakeShadow :: GenDUG -> Maybe S.ShadowImplementation -> S.Operation -> [D.DUGArg] -> IO ShadowResult
> tryMakeShadow dug impl op args = 
>       case impl of
>           Just shadowImpl -> do
>               result <- runDynamic t dyn
>               case result of
>                   RunSuccess d _ -> return $ HasShadow d
>                   RunTypeFail  -> error "type mismatch."
>                   RunExcept e  ->
>                       case e of
>                           E.GuardFailed    -> return ShadowGuardFailed
>                           E.NotImplemented -> return $ HasShadow dyn
>           Nothing -> return NoShadow
>   where
>       dynArgs = map mkDyn args
>       mkDyn (S.Version i) = fromJust $ ((dug ^. D.operations) !! i) ^. D.node ^. shadow
>       mkDyn (S.NonVersion (S.IntArg i)) = toDyn $ i
>       mkDyn (S.NonVersion (S.BoolArg i)) = toDyn $ i
>       mkDyn (S.NonVersion (S.VersionParam i)) = toDyn $ i
>       -- the fromJust here is safe -- it only gets evaluated in the `just` branch above
>       (dyn, t) = runNode (fromJust impl) (op ^. S.opName) dynArgs
>
> -- try fix some arguments for a buffered operation from the state
> -- and return the new state

Model and Pre-condition checking
--------------------------------

The first thing to check is that there are enough nodes in the DUG to use as version arguments
This happens before any arguments are ever chosen

> -- choose a set of valid arguments for each Node
> validArgs :: BufferedOperation -> GenState -> [[BufferedNode]]
> validArgs bop st = do
>   v <- bop ^. bufArgs
>   return $ ((st ^. dug ^. D.operations) !!) <$> nodes v
>   where
>       living = st ^. livingNodes
>       observerInfants = st ^. observerNodes ^. infants
>       mutatorInfants = st ^. observerNodes ^. infants
>       observerPersistents = st ^. observerNodes ^. persistents
>       mutatorPersistents = st ^. observerNodes ^. persistents
>       nodes v = 
>           case (v, bop ^. persistent, bop ^. bufOp ^. S.opSig ^. S.opType) of
>               (Filled _, _, _) -> []
>               (Abstract (S.NonVersion _), _, _) -> []
>               (Abstract (S.Version    _), False, S.Observer) -> 
>                   St.toList $ living `St.intersection` observerInfants
>               (Abstract (S.Version    _), False, _) -> 
>                   St.toList $ living `St.intersection` mutatorInfants
>               (Abstract (S.Version    _), True, S.Observer) -> 
>                   St.toList $ living `St.intersection` (observerInfants `St.union` observerPersistents)
>               (Abstract (S.Version    _), True, _) -> 
>                   St.toList $ living `St.intersection` (mutatorInfants `St.union` mutatorPersistents)


> shadow2str :: Dynamic -> S.ImplType -> String
> shadow2str d t = 
>       case unsafePerformIO (runDynamic t d) of
>           RunSuccess _ v -> show v           
>           _              -> ""

API
---

> makeDUG :: S.Signature -> P.Profile -> Int -> IO GenDUG
> makeDUG s p size = do
>       st <- go size (emptyState s p)
>       return $ st ^. dug
>   where
>       go :: Int -> GenState -> IO GenState
>       go size st =
>           if size <= (st ^. dug ^. D.operations & length) then
>               return st
>           else do
>               st' <- inflateStep st
>               st'' <- deflateStep st'
>               go size st''


> gendug2dot :: S.Signature -> GenDUG -> Bool -> String -> IO ()
> gendug2dot s d b fName = 
>   if b then
>       D.dug2dot' d nodeLabel (const "") fName
>   else
>       D.dug2dot d fName
>   where
>       opName n = n ^. D.nodeOperation ^. S.opName
>       nodeLabel n = 
>           opName n
>           ++ ": (" ++
>           fromMaybe "" (nodeShadowMaybe n)
>           ++ ")"
>       nodeShadowMaybe n = do
>           shImpl <- s ^. S.shadowImpl
>           let shTy = snd $ (shImpl ^. S.implOperations) M.! (opName n)
>           shDyn <- n ^. D.node ^. shadow
>           return $ shadow2str shDyn shTy

Buffer operations
----------------

Collection of useful operations on Buffered* types

> unFilled :: BufferedArg -> D.DUGArg
> unFilled (Filled a) = a
> unFilled _          = error "unFilled :: Recieved Abstract"
>
> isFilled :: BufferedArg -> Bool
> isFilled (Filled _) = True
> isFilled _          = False
