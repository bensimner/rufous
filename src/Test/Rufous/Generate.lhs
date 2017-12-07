> module Test.Rufous.Generate where
> import Control.Monad
> import System.Random
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

> data GenDug =
>    GenDug
>       { versions :: [Node]         -- the DUG built so far
>       , operations :: [BufferedOperation] -- operations yet to add
>       }
>       deriving (Eq, Show)

Whilst the Node's are primarily used for choosing Version arguments, they may be the result of an Observation,
and so do not represent an actual version of the data structure.

> type Node = (S.Operation, [DUGArg])

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

> data GenState = 
>   GenState
>       { dug :: GenDug
>       , sig :: S.Signature
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

> data BufferedOperation =
>    BufferedOperation
>       { bufOp     :: S.Operation
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

> emptyDug :: GenDug
> emptyDug = GenDug [] []
> 
> inflateDug :: GenState -> IO GenState
> inflateDug st = do
>    let p = profile st
>    let typeWeights = [ (inflateDug_Operation (P.normaliseWeights $ P.mutatorWeights p) (P.persistentMutationWeight p) st, P.pMutator p)
>                      , (inflateDug_Operation (P.normaliseWeights $ P.observerWeights p) (P.persistentObservationWeight p) st, P.pObserver p)
>                      , (inflateDug_Operation (P.normaliseWeights $ P.generatorWeights p) 0.0 st, P.pGenerator p) ]
>    join $ chooseRandom typeWeights
>
> inflateDug_Operation :: P.ProfileEntry -> Float -> GenState -> IO GenState
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

> tryDeflate :: GenState -> IO GenState
> tryDeflate state = do
>    let state' = state { dug=d { operations=[] } }
>    deflate (operations d) state'
>    where
>       d = dug state
>       deflate :: [BufferedOperation] -> GenState -> IO GenState
>       deflate (op : ops) st = do
>          (op', st') <- tryCommitOp op st
>          st'' <- deflate ops st'
>          let d' = dug st''
>          let ops' = operations d'
>          case op' of
>             Just op'' -> return $ st'' { dug=d' { operations=op'' : ops' } }
>             Nothing   -> return $ st'' { dug=d' { operations=ops' } }
>       deflate [] st = return st

Given a buffered operation, it can attempt to be committed to the DUG
    - If each remaining Arg in the operation can be committed then:
        - The operation can be removed from the buffer
        - The result can be added to the list of nodes
    - If not all of them can:
        - Commit those that can be
        - And return the partially applied operation to the buffer

> tryCommitOp :: BufferedOperation -> GenState -> IO (Maybe BufferedOperation, GenState)
> tryCommitOp bOp st = do
>    (args, rem) <- go (remaining bOp)
>    let newArgs = bufArgs bOp ++ args
>    if null rem then do
>       let versNode = (bufOp bOp, newArgs)
>       st' <- generateNodeState st (length vs)
>       return (Nothing, st' `withVersions` (vs ++ [versNode]))
>    else do
>       let bufOp' = BufferedOperation (bufOp bOp) (bufArgs bOp ++ args) (rem) (persistent bOp)
>       return (Just bufOp', st)
>    where
>       go :: [S.Arg] -> IO ([DUGArg], [S.Arg])
>       go (a : as) = do
>          maybeArg <- tryCommitArg a st
>          (args, rem) <- go as
>          case maybeArg of
>             Just dArg -> return (dArg : args, rem)
>             Nothing   -> return (args, a : rem)
>       go [] = return ([], [])
>       vs = versions (dug st)
> 
> tryCommitArg :: S.Arg -> GenState -> IO (Maybe DUGArg)
> tryCommitArg a st =
>    case a of
>       S.NonVersion -> do
>           r <- chooseNonVersion
>           return $ Just (NonVersion r)
>       S.Version -> do
>           let valid = validNodes st
>           if not . null $ valid then do  -- todo: prune observations and invalid operations
>               v <- chooseUniform valid
>               return $ Just (Version v)
>           else
>               return Nothing
> 
> -- generates the node state of Living/Dead
> generateNodeState :: GenState -> Int -> IO GenState
> generateNodeState st n = do
>   b <- randomFlag $ P.mortality (profile st)
>   if not b then
>       return $ st { livingNodes=n `St.insert` (livingNodes st) }
>   else
>       return st

To discover if a Node is valid:
    - check that it is a Version
    - check that it comes from the correct bin

> checkNode :: GenState -> Int -> Bool
> checkNode st ix = t /= S.Observer
>   where
>       d = dug st
>       vs = versions d
>       (op, _) = vs !! ix
>       t = S.opType $ S.sig op

Then collect these together

> validNodes :: GenState -> [Int]
> validNodes st = filter (checkNode st) ixs
>   where
>       vs = livingNodes st
>       ixs = [0 .. (length vs) - 1]

This deflation algorithm has many problems:
    1. Model state and pre-conditions
        Currently it assumes all operations are valid so long as the types match
        To fix this it would be necessary to carry around a model of the version and perform pre-condition checks for each operation
        which could be expensive if we allow the buffer to grow large.

        Fix: bound the size of the buffer?
    2. Choosing version arguments
        Currently version arguments are chosen randomly, even if point 1) was satisifed there is an additional constraint of choosing
        versions that already have been mutated for picking persistent operations

        Fix: store whether each buffered operation is persistent, and whether each Node has already been mutated
    3. Choosing non-version arguments
        Currently for simplicity we assume all non-version arguments are Int
        Obviously in the real world this isn't true, the hard part is choosing a representation that allows that type to fluctuate (heterogenous lists?)

> generate :: S.Signature -> P.Profile -> IO GenDug
> generate s p = do
>    k <- randomRIO (5, 50)
>    let emptyState = GenState emptyDug s p St.empty St.empty St.empty St.empty St.empty
>    st <- build emptyState k
>    st' <- flatten st
>    return $ dug st'
>    where
>       build :: GenState -> Int -> IO GenState  -- I do not know why I need this?
>       build st 0 = return st
>       build st k = do
>          st' <- inflateDug st
>          build st' (k - 1)
>
>       -- now run tryDeflate until fixed point is hit
>       flatten :: GenState -> IO GenState
>       flatten st = do
>          st' <- tryDeflate st
>          if dug st == dug st' then
>             return st'
>          else
>             flatten st'
>

Conversion and interaction
==========================

To interact with other components of Rufous, the DUGs here must be transformed into something more graph-like.

> genDug2DUG :: GenDug -> D.DUG
> genDug2DUG gd =
>   D.DUG
>       { D.versions=vs
>       , D.operations=os
>       }
>   where
>       gdVersions = versions gd
>       vs = map (S.opName . fst) gdVersions
>       dugArg2Arg da = 
>           case da of
>               Version i    -> D.VersionNodeArg i
>               NonVersion i -> D.NonVersionArg i
>       op2DArgs i = snd $ gdVersions !! i
>       op2Args i = (i, map dugArg2Arg (op2DArgs i))
>       os = M.fromList [op2Args i | i <- [0 .. (length vs) - 1]]

To update the `GenState` objects:

> withVersions :: GenState -> [Node] -> GenState
> withVersions st vs = st { dug=(dug st) { versions=vs } }
