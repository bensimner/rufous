> module Test.Rufous.Generate where
> import Control.Monad
> import System.Random
>
> import qualified Data.Map as M
>
> import Test.Rufous.RndUtil
>
> import qualified Test.Rufous.Signature as S
> import qualified Test.Rufous.Profile as P

Representation
==============

The representation for a DUG during generation is simply a pair:
    - The set of already-created versions, that can be immediately used in operations
    - and the set of operations that have been chosen but haven't been committed into the DUG yet


This implementation chooses to use a lists for the sets of nodes/operations


> data GenDug =
>    GenDug
>       { versions :: [VersionNode]         -- the DUG built so far
>       , operations :: [BufferedOperation] -- operations yet to add
>       }
>       deriving (Eq, Show)

A Version node is a node in the DUG that has been created. 
It should be noted here that VersionNode's need not actually contain a Version, they may just be phantom nodes from observations.

> type VersionNode = (S.Operation, [DUGArg])

Each version has a generation state, containing information about operations performed on it.
A version could have no further mutations and be `Dead' or be `Mutated' by a future operation.
A version could also be `Visible' through an observation or `Hidden' with no observations over it

This information is carried along during generation, and is computable from the DUG, but isn't directly stored in the DUG
Instead associated information is stored in a state object that gets passed around:

> data GenState = 
>   GenState
>       { dug :: GenDug
>       , sig :: S.Signature
>       , profile :: P.Profile
>       , mutatorBins :: ([Int], [Int])  -- for Dead/Mutated
>       , observerBins :: ([Int], [Int]) -- for Hidden/Visible
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
>

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
> tryDeflate st = do
>    let d = dug st
>    (ops, vs) <- deflate (operations d) (versions d)
>    let d' = GenDug vs ops
>    return $ st { dug = d' }
>    where
>       deflate :: [BufferedOperation] -> [VersionNode] -> IO ([BufferedOperation], [VersionNode])
>       deflate (op : ops) vs = do
>          (op', vs') <- tryCommitOp op vs
>          (ops', vs'') <- deflate ops vs'
>          case op' of
>             Just op'' -> return (op'' : ops', vs'')
>             Nothing   -> return (       ops', vs'')
>       deflate [] vs = return ([], vs)
>
> tryCommitOp :: BufferedOperation -> [VersionNode] -> IO (Maybe BufferedOperation, [VersionNode])
> tryCommitOp bOp vs = do
>    (args, rem) <- go (remaining bOp)
>    let newArgs = bufArgs bOp ++ args
>    if null rem then do
>       let versNode = (bufOp bOp, newArgs)
>       return (Nothing, versNode : vs)
>    else do
>       let bufOp' = BufferedOperation (bufOp bOp) (bufArgs bOp ++ args) (rem) (persistent bOp)
>       return (Just bufOp', vs)
>    where
>       go :: [S.Arg] -> IO ([DUGArg], [S.Arg])
>       go (a : as) =
>          case a of
>             S.NonVersion -> do
>                r <- chooseNonVersion
>                (args, rem) <- go as
>                return (NonVersion r : args, rem)
>             S.Version ->
>                -- todo: Compute subgraph of vs that is applicable here
>                if not $ null vs then do
>                   v <- chooseUniform $ [0 .. (length vs) - 1]
>                   (args, rem) <- go as
>                   return (Version v : args, rem)
>                else
>                   return ([], [])
>
>       go [] = return ([], [])

This deflation algorithm has many problems:
    1. Model state and pre-conditions
        Currently it assumes all operations are valid so long as the types match
        To fix this it would be necessary to carry around a model of the version and perform pre-condition checks for each operation
        which could be expensive if we allow the buffer to grow large.

        Fix: bound the size of the buffer?
    2. Choosing version arguments
        Currently version arguments are chosen randomly, even if point 1) was satisifed there is an additional constraint of choosing
        versions that already have been mutated for picking persistent operations

        Fix: store whether each buffered operation is persistent, and whether each VersionNode has already been mutated
    3. Choosing non-version arguments
        Currently for simplicity we assume all non-version arguments are Int
        Obviously in the real world this isn't true, the hard part is choosing a representation that allows that type to fluctuate (heterogenous lists?)

> generate :: S.Signature -> P.Profile -> IO GenDug
> generate s p = do
>    k <- randomRIO (5, 25)
>    let emptyState = GenState emptyDug s p ([], []) ([], [])
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
