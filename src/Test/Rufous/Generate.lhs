> module Test.Rufous.Generate where
> import Control.Monad
> import System.Random
> import Data.List
> import Debug.Trace
>
> import qualified Data.Map as M
> import qualified Data.Set as St
>
> import Test.Rufous.RndUtil
>
> import qualified Test.Rufous.Signature as S
> import qualified Test.Rufous.Profile as P
> import qualified Test.Rufous.DUG as D

Representation
==============

The representation for a DUG during generation is simply a pair:
    - The set of already-created versions, that can be immediately used in operations
    - and the set of operations that have been chosen but haven't been committed into the DUG yet

This implementation chooses to use a lists for the sets of nodes/operations

> data GenDug st =
>    GenDug
>       { versions :: [Node st]                 -- the DUG built so far
>       , operations :: [BufferedOperation st]  -- operations yet to add
>       }
>       deriving (Eq, Show)

Whilst the Node's are primarily used for choosing Version arguments, they may be the result of an Observation,
and so do not represent an actual version of the data structure.

> data Node st = 
>   Node 
>       { nodeOperation :: (S.Operation st) 
>       , nodeArgs :: [DUGArg] 
>       , nodeStates :: [st]
>       }
> instance Eq (Node st) where
>   (Node a b _) == (Node a' b' _)    = (a == a') && (b == b')
> instance Show (Node st) where
>   show (Node a b _) = "(" ++ show a ++ ". " ++ show b ++ ")"

Generation State 
----------------

Each version has a generation state, containing information about operations performed on it.
In order to generate persistent applications, of both mutators and observers, we need to track the persistent nature of each state.
There are 3 states per node - Persistent, Dead, Ephemeral, Infant
    - A Persistent node is one that either has been or will be mutated by multiple other operations
    - An Ephemeral node is one that should be mutated only once
    - An Infant node is one that has been created and has no mutations yet
    Equivalenty there are these 3 states for observation

Nodes themselves can also have a state:
    - A Dead node is one that should not be mutated
    - Living nodes are ones that should be mutated furthur

    This state allows us to control the mortality of the generated DUG, just by tweaking the initial probability of each state
    With the caveat that this will only allow control over killing nodes, not preventing new ones!
        (A low mortality with high generator probability can not be stopped with this method alone!)

A version could have no further mutations and be `Dead' or be `Mutated' by a future operation.
A version could also be `Visible' through an observation or `Hidden' with no observations over it

This information is carried along during generation, and is computable from the DUG, but isn't directly stored in the DUG
Instead associated information is stored in a state object that gets passed around:

> data GenState st = 
>   GenState
>       { dug :: GenDug st
>       , sig :: S.Signature st
>       , profile :: P.Profile
>       , livingNodes :: St.Set Int
>       , mutatorInfants :: St.Set Int
>       , mutatorPersistents :: St.Set Int
>       , observerInfants :: St.Set Int
>       , observerPersistents :: St.Set Int
>       }

The Arguments of a DUG are either arcs between Version's on the DUG or a hard-coded non-version argument
For simplicity we assume all non-version arguments are integers for now.

> data DUGArg = Version Int | NonVersion Int
>    deriving (Eq, Show)

Operations still to be committed are /buffered/
A Buffered operation consists of the name of the operation, the partially committed arguments, the set of remaining argument types (Version | NonVersion)
    and whether the committed operation should be a persistent application or not

> data BufferedOperation st =
>    BufferedOperation
>       { bufOp     :: S.Operation st
>       , bufArgs   :: [DUGArg]
>       , remaining :: [S.Arg]
>       , persistent :: Bool
>       }
>       deriving (Eq, Show)

Generation
==========

The generation algorithm is relatively straightforward/simple for now:
    - Start with the empty DUG
    - inflate the DUG by buffering as many operations as desired
    - deflate the DUG by committing each of the operations

Inflation
---------

For generating operations it's important to pick operations that conform to the given Profile
This is achieved by picking each operation with probability of their weights:
    - If given a profile [("a", 1/2), ("b": 1/6), ("c": 2/6)]
        then the final DUG with n operations would have roughly n/2 `a' operations, n/6 `b' operations and n/3 `c' operations
    - Capture persistence/mortality must be done at the deflation stage.

> emptyDug :: GenDug st
> emptyDug = GenDug [] []
> 
> inflateDug :: GenState st -> IO (GenState st)
> inflateDug st = do
>    let p = profile st
>    let typeWeights = [ (inflateDug_Operation (P.normaliseWeights $ P.mutatorWeights p) (P.persistentMutationWeight p) st, P.pMutator p)
>                      , (inflateDug_Operation (P.normaliseWeights $ P.observerWeights p) (P.persistentObservationWeight p) st, P.pObserver p)
>                      , (inflateDug_Operation (P.normaliseWeights $ P.generatorWeights p) 0.0 st, P.pGenerator p) ]
>    join $ chooseRandom typeWeights
>
> inflateDug_Operation :: P.ProfileEntry -> Float -> GenState st -> IO (GenState st)
> inflateDug_Operation m p st = do
>    opName <- chooseOperation m
>    let op = S.operations (sig st) M.! opName
>    persistent <- randomFlag p
>    let o = BufferedOperation op [] (S.opArgs $ S.sig op) persistent
>    let d = dug st
>    let d' = d { operations=o : operations d }
>    return $ st { dug=d' }

Deflation
---------

The deflation step is the complex one, and the current (simple) alogorithm is as follows:
    - Given a DUG define a deflate function that tries to commit each of the operations
        - Do this by iterating over each buffered operation, and trying to pick version/non-version arguments for each
            - If it fails to pick an argument, commit what succeeded and add what remains back on the buffer
            - If all arguments get committed (and `remaining == []`) then commit the whole operation and remove it from the buffer
    - Compute the deflate function to a fixed point

> tryDeflate :: GenState st -> IO (GenState st)
> tryDeflate state = do
>    let state' = state `withOperations` []
>    deflate (operations (dug state)) state'
>    where
>       deflate :: [BufferedOperation st] -> GenState st -> IO (GenState st)
>       deflate (op : ops) st = do
>          (op', st') <- tryCommitOp op st
>          st'' <- deflate ops st'
>          let d'' = dug st''
>          let ops'' = operations d''
>          case op' of
>             Just op'' -> return $ st'' `withOperations` (op'' : ops'')
>             Nothing   -> return $ st'' `withOperations` ops''
>       deflate [] st = return st

Given a buffered operation, it can attempt to be committed to the DUG

> tryCommitOp :: BufferedOperation st -> GenState st -> IO (Maybe (BufferedOperation st), GenState st)
> tryCommitOp bOp st = do
>    (args, rem, st') <- go (remaining bOp) st
>    let newArgs = bufArgs bOp ++ args
>    if null rem then do
>       if checkPre st bOp newArgs then do
>           let versNode = Node (bufOp bOp) newArgs (createFSMState st bOp newArgs)
>           st'' <- generateNewState bOp st' versNode
>           return (Nothing, st'')
>       else do
>           return (Just bOp, st')   -- put it back on the queue
>    else do
>       let bufOp' = BufferedOperation (bufOp bOp) (bufArgs bOp ++ args) (rem) (persistent bOp)
>       return (Just bufOp', st')
>    where
>       -- try find a list of arguments that satisfy the precondition to bOp
>       go (a : as) st = do
>          (maybeArg, st') <- tryCommitArg bOp a st
>          case maybeArg of
>             Just dArg -> do
>               (args, rem, st'') <- go as st'
>               return (dArg : args, rem, st'')
>             Nothing   -> return ([], a : as, st')
>       go [] st = return ([], [], st)
> 
> tryCommitArg :: BufferedOperation st -> S.Arg -> GenState st -> IO (Maybe DUGArg, GenState st)
> tryCommitArg bOp a st =
>    case a of
>       S.NonVersion -> do
>           r <- chooseNonVersion
>           return (Just (NonVersion r), st)
>       S.Version -> do
>           let valid = validNodes bOp st
>           if not . St.null $ valid then do  -- todo: prune invalid operations
>               v <- chooseUniform valid
>               let st' = updateNodeState bOp st v
>               return (Just (Version v), st')
>           else
>               return (Nothing, st)
> 
> generateNewState :: BufferedOperation st -> GenState st -> Node st -> IO (GenState st)
> generateNewState bOp st n = do
>   let vs = versions (dug st)
>   let ix = length vs
>   st' <- generateNodeState st ix              -- write state for current node
>   let st'' = st' `withVersions` (vs ++ [n])   -- add that node to the available versions
>   return st''
> 
> updateNodeState :: BufferedOperation st -> GenState st -> Int -> GenState st
> updateNodeState bOp st v =
>   case (persistent bOp, S.opType $ S.sig $ bufOp bOp) of
>       (_, S.Generator) -> st
>       (False, S.Mutator) -> st `withoutMutatorInfant` v
>       (False, S.Observer) -> st `withoutObserverInfant` v
>       (True, S.Mutator) -> (st `withoutMutatorInfant` v) `withMutatorPersistent` v
>       (True, S.Observer) -> (st `withoutObserverInfant` v) `withObserverPersistent` v
>       
> -- generates the node state of Living/Dead
> generateNodeState :: GenState st -> Int -> IO (GenState st)
> generateNodeState st n = do
>   b <- randomFlag $ P.mortality (profile st)
>   if not b then
>       return $ st 
>           { livingNodes=n `St.insert` (livingNodes st)
>           , mutatorInfants=n `St.insert` (mutatorInfants st)
>           , observerInfants=n `St.insert` (observerInfants st)
>           }
>   else
>       return st

To manage the FSM during operation:

> createFSMState :: GenState st -> BufferedOperation st -> [DUGArg] -> [st]
> createFSMState st bOp args = [S.initialState (sig st)]
>
> checkPre :: GenState st -> BufferedOperation st -> [DUGArg] -> Bool
> checkPre st bOp args =
>       if null args then
>           True
>       else
>           any (S.pre (bufOp bOp)) stateargs
>   where
>       stateargs = prods $ map dugarg2stateargs args
>       dugarg2stateargs a = 
>           case a of
>               Version i -> S.VersionArg <$> nodeStates (versions (dug st) !! i)
>               NonVersion k -> [S.IntArg k]
>       prods (xs : xss) = [x : xss' | x <- xs, xss' <- prods xss]
>       prods [] = []

To collect valid nodes, pick one randomly from the correct "bin" of nodes

> validNodes :: BufferedOperation st -> GenState st -> St.Set Int
> validNodes bOp st = livingNodes st `St.intersection` bucket
>   where
>       bucket =
>           case ((S.opType $ S.sig (bufOp bOp)), persistent bOp) of
>               (S.Mutator, True) -> mutatorInfants st `St.union` mutatorPersistents st
>               (S.Mutator, False) -> mutatorInfants st
>               (S.Observer, True) -> observerInfants st `St.union` observerPersistents st
>               (S.Observer, False) -> observerInfants st

This deflation algorithm has many problems:
    1. Model state and pre-conditions
        To enable pre-conditions each node must carry around state and 
        to check the pre-condition we must first generate a full set of parameters before checking.

        As the DUG grows this information will become unmanageable.

        Solution: Limit number of nodes that are available for future operations.
    3. Choosing non-version arguments
        Currently for simplicity we assume all non-version arguments are Int
        Obviously in the real world this isn't true, the hard part is choosing a representation that allows that type to fluctuate (heterogenous lists?)

> generate :: S.Signature st -> P.Profile -> (Int, Int) -> IO (GenDug st)
> generate s p sz = do
>    k <- randomRIO sz
>    let emptyState = GenState emptyDug s p St.empty St.empty St.empty St.empty St.empty
>    st <- build emptyState k
>    st' <- flatten st
>    return $ dug st'
>    where
>       build :: GenState st -> Int -> IO (GenState st)  -- I do not know why I need this?
>       build st 0 = return st
>       build st k = do
>          st' <- inflateDug st
>          build st' (k - 1)
>
>       -- now run tryDeflate until fixed point is hit
>       flatten :: GenState st -> IO (GenState st)
>       flatten st = do
>          st' <- tryDeflate st
>          let n = length $ versions $ dug st
>          let n' = length $ versions $ dug st'
>          if n == n' then
>             return st'
>          else
>             flatten st'
>

Conversion and interaction
==========================

To interact with other components of Rufous, the DUGs here must be transformed into something more graph-like.

> genDug2DUG :: GenDug st -> D.DUG
> genDug2DUG gd =
>   D.DUG
>       { D.versions=vs
>       , D.operations=os
>       }
>   where
>       gdVersions = versions gd
>       vs = [S.opName (nodeOperation n) | n <- gdVersions]
>       dugArg2Arg da = 
>           case da of
>               Version i    -> D.VersionNodeArg i
>               NonVersion i -> D.NonVersionArg i
>       op2DArgs i = nodeArgs $ gdVersions !! i
>       op2Args i = (i, map dugArg2Arg (op2DArgs i))
>       os = M.fromList [op2Args i | i <- [0 .. (length vs) - 1]]

To update the `GenState` objects:

> withVersions :: GenState st -> [Node st] -> GenState st
> withVersions st vs = st { dug=(dug st) { versions=vs } }
> withOperations st os = st { dug=(dug st) { operations=os } }
> withoutMutatorInfant :: GenState st -> Int -> GenState st
> withoutMutatorInfant st v = st { mutatorInfants=v `St.delete` (mutatorInfants st) }
> withoutObserverInfant st v = st { observerInfants=v `St.delete` (observerInfants st) }
> withMutatorPersistent st v = st { mutatorPersistents=v `St.insert` (mutatorPersistents st) }
> withObserverPersistent st v = st { observerPersistents=v `St.insert` (observerPersistents st) }
