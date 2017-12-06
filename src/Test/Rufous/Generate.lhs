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

> data GenDug =
>    GenDug
>       { versions :: [VersionNode]         -- the DUG built so far
>       , operations :: [BufferedOperation] -- operations yet to add
>       }
>       deriving (Eq, Show)

(Temporarily) a version is just an operation tagged with each argument

> type VersionNode = (String, [DUGArg])

The Arguments of a DUG are either arcs between Version's on the DUG or a hard-coded non-version argument
For simplicity we assume all non-version arguments are integers for now.

> data DUGArg = Version Int | NonVersion Int
>    deriving (Eq, Show)

Operations still to be committed are /buffered/
A Buffered operation consists of the name of the operation, the partially committed arguments and the set of remaining argument types (Version | NonVersion)

> data BufferedOperation =
>    BufferedOperation
>       { bufOpName :: String
>       , bufArgs   :: [DUGArg]
>       , remaining :: [S.Arg]
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

> -- add a thing to the DUG
> inflateDug :: S.Signature -> P.Profile -> GenDug -> IO GenDug
> inflateDug s p d = do
>    let typeWeights = [ (inflateDug_Operation s p (P.normaliseWeights $ P.mutatorWeights p) d, P.pMutator p)
>                      , (inflateDug_Operation s p (P.normaliseWeights $ P.observerWeights p) d, P.pObserver p)
>                      , (inflateDug_Operation s p (P.normaliseWeights $ P.generatorWeights p) d, P.pGenerator p) ]
>    join $ chooseRandom typeWeights
>
> inflateDug_Operation :: S.Signature -> P.Profile -> P.ProfileEntry -> GenDug -> IO GenDug
> inflateDug_Operation s p m d = do
>    opName <- chooseOperation m
>    let o = BufferedOperation opName [] (S.opArgs $ S.sig $ S.operations s M.! opName)
>    return $ d { operations=o : operations d }

Deflation
---------

The deflation step is the complex one, and the current (simple) alogorithm is as follows:
    - Given a DUG define a deflate function that tries to commit each of the operations
        - Do this by iterating over each buffered operation, and trying to pick version/non-version arguments for each
            - If it fails to pick an argument, commit what succeeded and add what remains back on the buffer
            - If all arguments get committed (and `remaining == []`) then commit the whole operation and remove it from the buffer
    - Compute the deflate function to a fixed point

> tryDeflate :: GenDug -> IO GenDug
> tryDeflate d = do
>    (ops, vs) <- new (operations d) (versions d)
>    return $ GenDug vs ops
>    where
>       new :: [BufferedOperation] -> [VersionNode] -> IO ([BufferedOperation], [VersionNode])
>       new (op : ops) vs = do
>          (op', vs') <- tryCommitOp op vs
>          (ops', vs'') <- new ops vs'
>          case op' of
>             Just op'' -> return (op'' : ops', vs'')
>             Nothing   -> return (       ops', vs'')
>       new [] vs = return ([], vs)
>
> tryCommitOp :: BufferedOperation -> [VersionNode] -> IO (Maybe BufferedOperation, [VersionNode])
> tryCommitOp bOp vs = do
>    (args, rem) <- go (remaining bOp)
>    let newArgs = bufArgs bOp ++ args
>    if null rem then
>       let versNode = (bufOpName bOp, newArgs)
>       in return (Nothing, versNode : vs)
>    else do
>       let bufOp' = BufferedOperation (bufOpName bOp) (bufArgs bOp ++ args) (rem)
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
>    d' <- build emptyDug k
>    flatten d'
>    where
>       build :: GenDug -> Int -> IO GenDug  -- I do not know why I need this?
>       build d 0 = return d
>       build d k = do
>          d' <- inflateDug s p d
>          build d' (k - 1)
>
>       -- now run tryDeflate until fixed point is hit
>       flatten :: GenDug -> IO GenDug
>       flatten d = do
>          d' <- tryDeflate d
>          if d == d' then
>             return d'
>          else
>             flatten d'
>
